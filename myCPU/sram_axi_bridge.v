`include "mycpu.h"

module sram_axi_bridge
(
	input wire clk,
	input wire reset,

	//icache
	input  wire         icache_rd_req    ,
	input  wire [  2:0] icache_rd_type   , //行3'b100对应cache miss，字3'b010对应uncached请求，可直接传给arsize
	input  wire [ 31:0] icache_rd_addr   ,
	output wire         icache_rd_rdy    ,
	output wire         icache_ret_valid ,
	output wire         icache_ret_last  ,
	output wire [ 31:0] icache_ret_data  ,

	//dcache
	input  wire         dcache_rd_req    ,
	input  wire [  2:0] dcache_rd_type   , //字节3'b000 半字3'b001 字3'b010 行3'b100，可直接传给arsize
	input  wire [ 31:0] dcache_rd_addr   ,
	output wire         dcache_rd_rdy    ,
	output wire         dcache_ret_valid ,
	output wire         dcache_ret_last  ,
	output wire [ 31:0] dcache_ret_data  ,
	input  wire         dcache_wr_req    , //dcache所有写请求都先存在bridge里，之后由bridge向总线发出写请求
	input  wire [  2:0] dcache_wr_type   ,
	input  wire [ 31:0] dcache_wr_addr   ,
	input  wire [  3:0] dcache_wr_wstrb  ,
	input  wire [127:0] dcache_wr_data   ,
	output wire         dcache_wr_rdy    ,

	// axi interface 不能修改
	output wire[3 :0] arid   ,
	output wire[31:0] araddr ,
	output wire[7 :0] arlen  ,
	output wire[2 :0] arsize ,
	output wire[1 :0] arburst,
	output wire[1 :0] arlock ,
	output wire[3 :0] arcache,
	output wire[2 :0] arprot ,
	output wire       arvalid,
	input  wire       arready,

	input  wire[3 :0] rid   ,
	input  wire[31:0] rdata ,
	input  wire[1 :0] rresp ,
	input  wire       rlast ,
	input  wire       rvalid,
	output wire       rready,

	output wire[3 :0] awid   ,
	output wire[31:0] awaddr ,
	output wire[7 :0] awlen  ,
	output wire[2 :0] awsize ,
	output wire[1 :0] awburst,
	output wire[1 :0] awlock ,
	output wire[3 :0] awcache,
	output wire[2 :0] awprot ,
	output wire       awvalid,
	input  wire       awready,
	
	output wire[3 :0] wid   ,
	output wire[31:0] wdata ,
	output wire[3 :0] wstrb ,
	output wire       wlast ,
	output wire       wvalid,
	input  wire       wready,
	
	input  wire[3 :0] bid   ,
	input  wire[1 :0] bresp ,
	input  wire       bvalid,
	output wire       bready
);
wire       read_inst             ; //当前正在读指令握手，优先级低于read_data
wire       read_data             ; //当前正在读数据握手
wire       write_data            ; //当前正在写数据握手
reg        is_reading_inst       ; //read_inst正在握手，并不是在等返回数据
reg        is_reading_data       ; //read_data正在握手，并不是在等返回数据
wire       awhs                  ; //hs就是handshake
wire       whs                   ; //hs就是handshake
reg        awhs_r                ;
reg        whs_r                 ;
wire       awhs_flag             ;
wire       whs_flag              ;
wire       write_data_addr_ok    ; //awhs_flag && whs_flag && words_to_write == 0
wire       icache_rd_uncached    ;
wire       dcache_rd_uncached    ;
wire       dcache_wr_uncached    ;
reg        uncached_not_finish   ;
reg [ 2:0] words_to_write        ; //还有多少个字等待通过w通道写入axi
reg        wr_req                ; //dcache_wr_req的寄存器，表示有需要握手的写请求，握手后复位，用于awvalid/wvalid的置位
reg        wr_req_r              ;
reg        wr_uncached           ;
reg [31:0] wr_addr               ;
reg [ 2:0] wr_type               ;
reg [ 3:0] wr_wstrb              ;
reg        has_unsolved_write    ;
reg [31:0] dcache_wr_data_r [3:0];

//uncached
assign icache_rd_uncached = (icache_rd_type != 3'b100) && icache_rd_req;
assign dcache_rd_uncached = (dcache_rd_type != 3'b100) && dcache_rd_req;
assign dcache_wr_uncached = (dcache_wr_type != 3'b100) && dcache_wr_req;

//描述当前状态
assign read_inst      = icache_rd_req && (!(icache_rd_uncached && uncached_not_finish) || bvalid) && !is_reading_data && !read_data;
assign read_data      = dcache_rd_req && (!(dcache_rd_uncached && uncached_not_finish) || bvalid) && !is_reading_inst;
assign write_data     = wr_req;

//icache
assign icache_rd_rdy    = arready && read_inst; //读请求已经被接收，arready & read_inst，内存已经接收读请求，且是接收指令读请求
assign icache_ret_valid = ~rid[0] & rvalid;
assign icache_ret_last  = ~rid[0] & rlast;
assign icache_ret_data  = rdata;

//dcache
assign dcache_rd_rdy    = arready && read_data; //读请求已经被接收
assign dcache_ret_valid = rid[0] & rvalid;
assign dcache_ret_last  = rid[0] & rlast;
assign dcache_ret_data  = rdata;
assign dcache_wr_rdy    = (words_to_write[2:1] == 2'b0) && !(uncached_not_finish/*  && dcache_wr_uncached */) || bvalid; //只剩一个字或已经写完，以及uncached阻塞

assign awhs = awvalid && awready;
assign whs  =  wvalid &&  wready;

assign arid    = read_data ? 4'd1 : 4'd0;
assign araddr  = read_data ? dcache_rd_addr : icache_rd_addr;
assign arlen   = read_data ? (dcache_rd_uncached ? 8'h00 : 8'h03) : (icache_rd_uncached ? 8'h00 : 8'h03); //icache和dcache的rd请求都需要1+3拍
assign arsize  = 3'b010; //修正：站在axi来看，无论是否为uncached，一次传输还是4个字节
assign arburst = 2'b01; //事务类型都是INCR类型
assign arlock  = 2'b0;
assign arcache = 4'b0;
assign arprot  = 3'b0;
assign arvalid = read_inst || read_data;

assign awhs_flag = awhs || awhs_r;
assign whs_flag  =  whs ||  whs_r;

assign write_data_addr_ok = awhs_flag && whs_flag && write_data && (words_to_write == 3'b000);

assign rready = 1'b1;

assign awid = 4'b1;
assign awaddr = wr_req ? wr_addr : dcache_rd_addr;
assign awlen = wr_uncached ? 8'h00 : 8'h03; //写cache行时要4拍，uncached一拍即可
assign awsize = 3'b010; //dcache_wr_type; //修正：理由同arsize
assign awburst = 2'b01;
assign awlock = 2'b0;
assign awcache = 4'b0;
assign awprot = 3'b0;
assign awvalid = write_data && !awhs_r;

assign wid = 4'b1;
assign wdata = wr_uncached ? dcache_wr_data_r[3] : dcache_wr_data_r[words_to_write-3'b001];
assign wstrb = wr_wstrb;
assign wlast = wr_uncached || uncached_not_finish || (words_to_write == 3'b001);
assign wvalid = (write_data || has_unsolved_write) && (words_to_write != 3'b000);

assign bready = 1'b1;

always @(posedge clk) begin
	if (reset) begin
		awhs_r <= 1'b0;
	end
	else if (write_data_addr_ok) begin
		awhs_r <= 1'b0;
	end
	else if (awhs) begin
		awhs_r <= 1'b1;
	end
	
	if (reset) begin
		whs_r <= 1'b0;
	end
	else if (write_data_addr_ok) begin
		whs_r <= 1'b0;
	end
	else if (whs) begin
		whs_r <= 1'b1;
	end
	
	if (reset) begin
		is_reading_inst <= 1'b0;
	end
	else if (read_inst) begin
		is_reading_inst <= !(arready && read_inst);
	end

	if (reset) begin
		is_reading_data <= 1'b0;
	end
	else if (read_data) begin
		is_reading_data <= !(arready & read_data);
	end

	if (reset) begin
		uncached_not_finish <= 1'b0;
	end
	else if (bvalid) begin
		uncached_not_finish <= 1'b0;
	end
	else if (wr_uncached) begin	
		uncached_not_finish <= 1'b1;
	end

	if (reset) begin
		words_to_write <= 3'b000;
	end
	else if (wr_req && !wr_req_r) begin
		words_to_write <= wr_uncached ? 3'b001 : 3'b100;
	end
	else if (whs_flag && words_to_write != 3'b000 && !bvalid) begin //加了一个!bvalid，因为可能出现bvalid和下一次write请求同时到来的情况，如果此时的写请求words_to_write = 1，那就会误以为transfer_start，导致words_to_write变为0
		words_to_write <= words_to_write - 3'b001;
	end

/* 	if (reset) begin
		wr_req      <=  1'b0;
		wr_uncached <=  1'b0;
		wr_type     <=  3'b0;
		wr_wstrb    <=  4'b0;
		wr_addr     <= 32'b0;
	end
	else if (dcache_wr_req) begin
		wr_req              <= 1'b1                  ;
		wr_uncached         <= dcache_wr_uncached    ;
		wr_type             <= dcache_wr_type        ;
		wr_wstrb            <= dcache_wr_wstrb       ;
		wr_addr             <= dcache_wr_addr        ;
		dcache_wr_data_r[3] <= dcache_wr_data[ 31: 0];
		dcache_wr_data_r[2] <= dcache_wr_data[ 63:32];
		dcache_wr_data_r[1] <= dcache_wr_data[ 95:64];
		dcache_wr_data_r[0] <= dcache_wr_data[127:96];
	end
	else if (write_data_addr_ok) begin
		wr_req <= 1'b0;
	end
	else if (bvalid) begin
		wr_uncached <=  1'b0;
		wr_type     <=  3'b0;
		wr_wstrb    <=  4'b0;
		wr_addr     <= 32'b0;
	end */
	if (reset) begin
		wr_req <= 1'b0;
	end
	else if (write_data_addr_ok) begin
		wr_req <= 1'b0;
	end
	else if (dcache_wr_req) begin
		wr_req <= 1'b1;
	end

	if (reset) begin
		wr_uncached <=  1'b0;
		wr_type     <=  3'b0;
		wr_wstrb    <=  4'b0;
		wr_addr     <= 32'b0;
	end
	else if (dcache_wr_req) begin
		wr_uncached         <= dcache_wr_uncached    ;
		wr_type             <= dcache_wr_type        ;
		wr_wstrb            <= dcache_wr_wstrb       ;
		wr_addr             <= dcache_wr_addr        ;
		dcache_wr_data_r[3] <= dcache_wr_data[ 31: 0];
		dcache_wr_data_r[2] <= dcache_wr_data[ 63:32];
		dcache_wr_data_r[1] <= dcache_wr_data[ 95:64];
		dcache_wr_data_r[0] <= dcache_wr_data[127:96];
	end
	else if (bvalid) begin
		wr_uncached <=  1'b0;
		wr_type     <=  3'b0;
		wr_wstrb    <=  4'b0;
		wr_addr     <= 32'b0;
	end

	if (reset) begin
		has_unsolved_write <= 1'b0;
	end
	else if (wr_req) begin
		has_unsolved_write <= 1'b1;
	end
	else if (bvalid) begin
		has_unsolved_write <= 1'b0;
	end

	wr_req_r <= wr_req;
end

endmodule