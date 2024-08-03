`define ADDR_SIZE           32
`define WAY_NUM             2                       //路数
`define LINE_NUM            256                     //组数，每 ??路的行数
`define BYTES_PERLINE       16                      //每行字节 ??

`define OFFSET_WIDTH        4 //$clog2(BYTES_PERLINE)               //偏移位宽
`define INDEX_WIDTH         8 //$clog2(LINE_NUM)                    //索引位宽
`define TAG_WIDTH           20 //ADDR_SIZE-OFFSET_WIDTH-INDEX_WIDTH  //标签位宽
`define BITS_PERLINE        BYTES_PERLINE*8                     //每行bits
`define WORDS_PERLINE       4 //BYTES_PERLINE/4                     //每行words
`define BANK_NUM_WIDTH      2 //$clog2(WORDS_PERLINE)               //bank编号位宽
//以上参数可替换到下面，方便后续更改 ? 下面接口的位宽使用的是设计实战的定 ??

module cache(
    input               clk   ,
    input               resetn,

    //与流水线交互接口
    input               valid   ,
    input               op      ,
    input [        7:0] index   ,
    input [       19:0] tag     ,
    input [        3:0] offset  ,
    input [        3:0] wstrb   ,
    input [       31:0] wdata   ,
    input               uncached,

    output              addr_ok,
    output              data_ok,
    output[       31:0] rdata  , 

    //与AXI总线交互接口
    output              rd_req ,
    output[        2:0] rd_type,
    output[       31:0] rd_addr,

    input               rd_rdy   ,
    input               ret_valid,
    input               ret_last ,
    input [       31:0] ret_data ,

    output              wr_req  ,
    output[        2:0] wr_type ,
    output[       31:0] wr_addr ,
    output[        3:0] wr_wstrb,
    output[      127:0] wr_data ,
    input               wr_rdy  ,

    //cacop
    input               cacop_req,
    input [        1:0] cacop_op
);
wire reset = ~resetn;
wire req;
wire stall;
wire way0_hit;
wire way1_hit;
wire true_hit; //way0_hit || way1_hit，只看tag是否命中
wire cache_hit; //当uncached和cacop时一定miss
wire cache_miss;
reg is_idle, is_lookup, is_miss, is_replace, is_refill;
reg wbf_is_idle,wbf_is_write;
reg replace_first;
wire hit_write;
reg wr_req_r;

//替换
wire replace_way;
wire [127:0] replace_data;

//cache表相 ??
wire [19:0] way0_tag, way1_tag;
wire        way0_v, way1_v;

//request buffer
reg                           req_op      ;
reg     [   `INDEX_WIDTH-1:0] req_index   ;
reg     [     `TAG_WIDTH-1:0] req_tag     ;
reg     [  `OFFSET_WIDTH-1:0] req_offset  ;
reg     [                3:0] req_wstrb   ;
reg     [     `ADDR_SIZE-1:0] req_wdata   ;
reg                           req_uncached;
wire    [`BANK_NUM_WIDTH-1:0] req_bank_num;
reg                           req_cacop_req;
reg     [                1:0] req_cacop_op;
wire req_cacop_op0, req_cacop_op1, req_cacop_op2;
assign req_cacop_op0 = req_cacop_req && (req_cacop_op == 2'b00);
assign req_cacop_op1 = req_cacop_req && (req_cacop_op == 2'b01);
assign req_cacop_op2 = req_cacop_req && (req_cacop_op == 2'b10);

//write buffer
reg                      wbf_way      ;
reg [`BANK_NUM_WIDTH-1:0] wbf_bank_num;
reg [   `INDEX_WIDTH-1:0] wbf_index   ; 
reg [                3:0] wbf_wstrb   ;
reg [     `ADDR_SIZE-1:0] wbf_wdata   ;

//miss buffer MISS/REPLACE/REFILL状 ? 用到该通路
reg [`TAG_WIDTH-1:0] miss_tag     ;
reg [         127:0] miss_data    ;
reg [           1:0] miss_ret_nums;
reg                  miss_way     ;
reg                  miss_v       ;
wire                 miss_d       ;
reg                  miss_true_hit;
reg [           7:0] miss_d_addr  ;

assign req = valid;

//cache ??
//tag_v 在REFILL状 ? 时，在ret_last == 1时更改tag_v
wire [7:0] tag_v_addr = // req_cacop_req                              ? req_index : ///////////////////这句应该可以注释掉
                        (is_lookup && (cache_miss || req_uncached)) ? req_index : 
                        is_refill                                  ? req_index : 
                        index;
wire [20:0] tag_v_wdata = (req_cacop_op0 || req_cacop_op2 || req_uncached) ? 21'b0 : {req_tag, 1'b1}; //这里手册上说的是将tag置为全0，我这里把valid也同时置0了，不然还要先读再存，要加通路
tag_v_ram tag_v_ram0(
    .addra(tag_v_addr                                      ),
    .clka (clk                                             ),
    .dina (tag_v_wdata                                     ),
    .douta({way0_tag,way0_v}                               ),
    .wea  (is_refill & ret_last & ~miss_way & ~req_uncached | ((req_cacop_op0 | req_cacop_op1) & ~req_offset[0] | (req_cacop_op2 | req_uncached) & way0_hit) & is_lookup)
);
tag_v_ram tag_v_ram1(
    .addra(tag_v_addr                                     ),
    .clka (clk                                            ),
    .dina (tag_v_wdata                                    ),
    .douta({way1_tag,way1_v}                              ),
    .wea  (is_refill & ret_last &  miss_way & ~req_uncached | ((req_cacop_op0 | req_cacop_op1) &  req_offset[0] | (req_cacop_op2 | req_uncached) & way1_hit) & is_lookup)
);
//d
reg [255:0] d0_regfile;
reg [255:0] d1_regfile;
wire [7:0] d_addr =/*  (is_lookup && cache_miss) ? req_index :  */
                    is_refill                 ? req_index : 
                    wbf_index;
always @(posedge clk) begin
    if(wbf_is_write) begin
        d0_regfile[d_addr] <= d0_regfile[d_addr] | ~wbf_way;
        d1_regfile[d_addr] <= d1_regfile[d_addr] |  wbf_way;
    end
    else if(is_refill && ret_last && !req_uncached) begin
        d0_regfile[d_addr] <= req_op & ~miss_way;
        d1_regfile[d_addr] <= req_op &  miss_way;
    end
end
//data
wire [7:0] data_addr   = wbf_is_write           ? wbf_index : 
                         (is_refill || is_miss) ? req_index : 
                         index;
wire [3:0] data_wstrb  = wbf_is_write ? wbf_wstrb : 
                         is_refill    ? 4'b1111   : 4'b0;
wire [31:0] mask;
wire [31:0] true_data  = (req_op && (req_bank_num == miss_ret_nums)) ? ((mask & req_wdata) | (~mask & ret_data)) : ret_data;
wire [31:0] data_wdata = is_refill ? true_data : wbf_wdata;

assign mask = {{8{req_wstrb[3]}}, {8{req_wstrb[2]}}, {8{req_wstrb[1]}}, {8{req_wstrb[0]}}} & {32{req_op}};

//bank写使 ??
wire we00, we01, we02, we03;
wire we10, we11, we12, we13;
assign we00 = (is_refill && !miss_way && !req_uncached && (miss_ret_nums == 2'b00) && ret_valid) || (wbf_is_write) && !wbf_way && (wbf_bank_num == 2'b00);
assign we01 = (is_refill && !miss_way && !req_uncached && (miss_ret_nums == 2'b01) && ret_valid) || (wbf_is_write) && !wbf_way && (wbf_bank_num == 2'b01);
assign we02 = (is_refill && !miss_way && !req_uncached && (miss_ret_nums == 2'b10) && ret_valid) || (wbf_is_write) && !wbf_way && (wbf_bank_num == 2'b10);
assign we03 = (is_refill && !miss_way && !req_uncached && (miss_ret_nums == 2'b11) && ret_valid) || (wbf_is_write) && !wbf_way && (wbf_bank_num == 2'b11);
assign we10 = (is_refill &&  miss_way && !req_uncached && (miss_ret_nums == 2'b00) && ret_valid) || (wbf_is_write) &&  wbf_way && (wbf_bank_num == 2'b00);
assign we11 = (is_refill &&  miss_way && !req_uncached && (miss_ret_nums == 2'b01) && ret_valid) || (wbf_is_write) &&  wbf_way && (wbf_bank_num == 2'b01);
assign we12 = (is_refill &&  miss_way && !req_uncached && (miss_ret_nums == 2'b10) && ret_valid) || (wbf_is_write) &&  wbf_way && (wbf_bank_num == 2'b10);
assign we13 = (is_refill &&  miss_way && !req_uncached && (miss_ret_nums == 2'b11) && ret_valid) || (wbf_is_write) &&  wbf_way && (wbf_bank_num == 2'b11);
//bank命名，data_bank_ramxy，其中x是路，y是bank ??
wire [31:0] way0_data0;
data_bank_ram data_bank_ram00(
    .addra(data_addr ),
    .clka (clk       ),
    .dina (data_wdata),
    .douta(way0_data0),
    .wea  (we00      )
);

wire [31:0] way0_data1;
data_bank_ram data_bank_ram01(
    .addra(data_addr ),
    .clka (clk       ),
    .dina (data_wdata),
    .douta(way0_data1),
    .wea  (we01      )
);

wire [31:0] way0_data2;
data_bank_ram data_bank_ram02(
    .addra(data_addr ),
    .clka (clk       ),
    .dina (data_wdata),
    .douta(way0_data2),
    .wea  (we02      )
);

wire [31:0] way0_data3;
data_bank_ram data_bank_ram03(
    .addra(data_addr ),
    .clka (clk       ),
    .dina (data_wdata),
    .douta(way0_data3),
    .wea  (we03      )
);

wire [127:0] way0_data = {way0_data3,way0_data2,way0_data1,way0_data0};

wire [31:0] way1_data0;
data_bank_ram data_bank_ram10(
    .addra(data_addr ),
    .clka (clk       ),
    .dina (data_wdata),
    .douta(way1_data0),
    .wea  (we10      )
);

wire [31:0] way1_data1;
data_bank_ram data_bank_ram11(
    .addra(data_addr ),
    .clka (clk       ),
    .dina (data_wdata),
    .douta(way1_data1),
    .wea  (we11      )
);

wire [31:0] way1_data2;
data_bank_ram data_bank_ram12(
    .addra(data_addr ),
    .clka (clk       ),
    .dina (data_wdata),
    .douta(way1_data2),
    .wea  (we12      )
);

wire [31:0] way1_data3;
data_bank_ram data_bank_ram13(
    .addra(data_addr ),
    .clka (clk       ),
    .dina (data_wdata),
    .douta(way1_data3),
    .wea  (we13      )
);

wire [127:0] way1_data = {way1_data3,way1_data2,way1_data1,way1_data0};

assign stall = !op && ((req_op && cache_hit && (index == req_index) && (req_offset == offset))
                     || wbf_is_write);/* (wbf_is_write && (wbf_bank_num == offset[3:2]))); */
//!stall = op || !((req_op && cache_hit && (index == req_index) && (req_offset == offset)) || wbf_is_write)
//op || !stall = !stall = op || !((req_op && cache_hit && (index == req_index) && (req_offset == offset)) || wbf_is_write)
/* !((req_op && cache_hit && (index == req_index) && (req_offset == offset)) || wbf_is_write) = !() && !wbf_is_write
op || !()        &&        op || !wbf_is_write */

//tag compare
assign way0_hit = way0_v && (way0_tag == req_tag) && is_lookup;
assign way1_hit = way1_v && (way1_tag == req_tag) && is_lookup; 

assign cache_hit = (way0_hit || way1_hit) && is_lookup && !req_cacop_req && !req_uncached; //只要uncached或者req_caacop_req就一定miss  //3.323
assign cache_miss = ~cache_hit;
assign true_hit = way0_hit || way1_hit;

//data select
wire [31:0] way0_load_word;
wire [31:0] way1_load_word;

assign way0_load_word = way0_data[req_offset[3:2]*32 +: 32];
assign way1_load_word = way1_data[req_offset[3:2]*32 +: 32];

//CPU流水线接 ??
assign rdata = {32{way0_hit }} & way0_load_word
             | {32{way1_hit }} & way1_load_word
             | {32{is_refill}} & ret_data;
/* assign addr_ok = (is_idle || cache_hit && (op || !stall)) && !wbf_is_write; */ ////////////////////第二种阻塞，我把wbf_is_write拓宽了，因为data_bank_ram前一拍传地址，后一拍才能读到数据
/* wire addr_ok_h = cache_hit && !wbf_is_write && (op || !((index == req_index) && (req_offset == offset))); */
//方式1 
assign addr_ok = is_idle && !wbf_is_write || cache_hit && !wbf_is_write && (op || !((index == req_index) && (req_offset == offset)));
assign data_ok = cache_hit && !req_uncached 
              || is_lookup && req_op 
              || is_refill && ret_valid && !req_op && (req_uncached || (miss_ret_nums == req_offset[3:2]));
//AXI接口
assign rd_req = is_replace && !(req_uncached && req_op) && !req_cacop_req; //写请求不发读信号
assign rd_type = req_uncached ? 3'b010 : 3'b100;
assign rd_addr = {req_tag, req_index, (req_uncached ? req_offset : 4'b0)};
assign wr_req = wr_req_r;
assign wr_type = req_uncached ? (req_wstrb == 4'b1111 ? 3'b010 : req_wstrb == 4'b0011 ? 3'b001 : 3'b000) : 3'b100; //目前是这样的
assign wr_addr = req_uncached ? {req_tag, req_index, req_offset} : {miss_tag, req_index, 4'b0};
assign wr_wstrb = req_uncached ? req_wstrb : 4'b1111; //我们目前wr_type = 3'b1000时，该信号无 ??
assign wr_data = req_uncached ? {96'b0, req_wdata} : 
                 replace_first ? replace_data : miss_data;

always @(posedge clk) begin
    if (is_miss && wr_rdy) begin
/*         wr_req_r <= miss_v & miss_d & (!req_cacop_op2 | miss_true_hit) | req_uncached & req_op; //有效且脏才需要写 */
        wr_req_r <= req_uncached ? req_op : miss_v & miss_d & (!req_cacop_op2 | miss_true_hit);
    end
    else if (wr_rdy) begin
        wr_req_r <= 1'b0;
    end
end

//替换
assign replace_way = req_cacop_req ? (req_cacop_op2 ? way1_hit : req_offset[0]) : way0_v; //如果当前是一个cacop op=1请求，替换的路换做req_offset[0];op=2，看哪路命中
assign replace_data = replace_way ? way1_data : way0_data; 

//request buffer
assign req_bank_num  = req_offset[3:2];
always @(posedge clk) begin
    if (reset) begin
        req_cacop_req <= 1'b0;
        req_cacop_op  <= 2'b0;
    end
    if (addr_ok && (req || cacop_req) && !stall) begin
        req_op          <= op        ;
        req_index       <= index     ;
        req_tag         <= tag       ;
        req_offset      <= offset    ;
        req_wstrb       <= wstrb     ;
        req_wdata       <= wdata     ;
        req_uncached    <= uncached  ;
        req_cacop_req   <= cacop_req ;
        req_cacop_op    <= cacop_op  ;
    end
end

//miss buffer
always @(posedge clk) begin
    if (reset) miss_ret_nums <= 1'b0;
    else if (is_refill && ret_valid && !req_uncached) begin
        miss_ret_nums <= miss_ret_nums + 1'b1;
    end
    if (is_lookup & cache_miss) begin
        miss_tag          <= replace_way ? way1_tag : way0_tag;
        miss_way          <= replace_way;
        miss_v            <= replace_way ? way1_v : way0_v;
        /* miss_d            <= replace_way ? d1_regfile[d_addr] : d0_regfile[d_addr]; */
        miss_true_hit     <= true_hit;
        miss_d_addr       <= req_index;
    end
/*     if (is_miss) begin //分开写是因为，可能上一次写刚从LOOKUP离开进入wbf_is_write，换言之现在是第二个写的LOOKUP（未命中）和上一次的wbf_is_write，因为wbf_is_write状态发写数据请求，此时读出来的replace_data还没有更新
        miss_data <= replace_data;
    end */
    if (replace_first) begin
        miss_data <= replace_data;
    end
end
assign miss_d = miss_way ? d1_regfile[miss_d_addr] : d0_regfile[miss_d_addr];

//write buffer
wire [31:0] wbf_true_data;
assign wbf_true_data = mask & req_wdata | ~mask & (wbf_is_write && (wbf_index == req_index) && (wbf_bank_num == req_bank_num) ? wbf_wdata : rdata);
always @(posedge clk) begin
    if(is_lookup && hit_write) begin
        wbf_way      <= way1_hit     ;
        wbf_bank_num <= req_bank_num ;
        wbf_index    <= req_index    ;
        wbf_wstrb    <= req_wstrb    ;
        wbf_wdata    <= wbf_true_data;
    end
end

assign hit_write = cache_hit & req_op;

//write buffer状 ? 机
always @(posedge clk) begin
    if (reset) begin
        wbf_is_idle     <= 1'b1;
        wbf_is_write    <= 1'b0;
    end
    else if(wbf_is_idle && hit_write) begin
        wbf_is_idle     <= 1'b0;
        wbf_is_write    <= 1'b1;
    end
    else if(wbf_is_write && !hit_write) begin
        wbf_is_write    <= 1'b0;
        wbf_is_idle     <= 1'b1;
    end
end

//主状态机
always @(posedge clk) begin
    if (reset) begin
        is_idle     <= 1'b1;
        is_lookup   <= 1'b0;
        is_miss     <= 1'b0;
        is_replace  <= 1'b0;
        is_refill   <= 1'b0;
    end
    else if(is_idle && (req || cacop_req) && !stall && !wbf_is_write) begin
        is_idle     <= 1'b0;
        is_lookup   <= 1'b1;
    end
    else if(is_lookup && (cache_hit && !(addr_ok && req) || req_cacop_op0)) begin
        is_lookup   <= 1'b0;
        is_idle     <= 1'b1;
    end
    else if(is_lookup && cache_miss && !req_cacop_op0) begin
        is_lookup   <= 1'b0;
        is_miss     <= 1'b1;
    end
    else if(is_miss && wr_rdy) begin
        is_miss     <= 1'b0;
        is_replace  <= 1'b1;
    end
    else if(is_replace && (rd_rdy || (req_uncached && req_op) || req_cacop_req)) begin
        is_replace  <= 1'b0;
        is_refill   <= 1'b1;
    end
    else if(is_refill && (ret_valid && ret_last || (req_uncached && req_op) || req_cacop_req)) begin
        is_refill   <= 1'b0;
        is_idle     <= 1'b1;
    end

    if (reset) begin
        replace_first <= 1'b0;
    end
    else if (is_miss && wr_rdy) begin
        replace_first <= 1'b1;
    end
    else if (is_replace) begin
        replace_first <= 1'b0;
    end
end

endmodule

`ifdef DIFFTEST_EN
module tag_v_ram
#( 
    parameter WIDTH = 21    ,
    parameter DEPTH = 256
)
( 
    input  [ 7:0]          addra   ,
    input                  clka    ,
    input  [20:0]          dina    ,
    output [20:0]          douta   ,
    input                  wea 
);

reg [20:0] mem_reg [255:0];
reg [20:0] output_buffer;

always @(posedge clka) begin
        if (wea) begin
            mem_reg[addra] <= dina;
            output_buffer <= dina;
        end
        else begin
            output_buffer <= mem_reg[addra];
        end
end

assign douta = output_buffer;

endmodule

module data_bank_ram
#(
    parameter WIDTH = 32    ,
    parameter DEPTH = 256
)
(
    input  [ 7:0]          addra   ,
    input                  clka    ,
    input  [31:0]          dina    ,
    output [31:0]          douta   ,
    input                  wea      
);

reg [31:0] mem_reg [255:0];
reg [31:0] output_buffer;

always @(posedge clka) begin
        if (wea) begin
            mem_reg[addra][31: 0] <= dina[31: 0];
            output_buffer <= dina[31:0]; ///////////////////////////////////
        end
        else begin
            output_buffer <= mem_reg[addra];
        end
end

assign douta = output_buffer;

endmodule
`endif