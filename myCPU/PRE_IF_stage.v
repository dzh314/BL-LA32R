`include "mycpu.h"

module pre_if_stage(
    input                          clk           ,
    input                          reset         ,
    //allwoin
    input                          fs_allowin    ,
    //brbus
    input  [`BR_BUS_WD       -1:0] br_bus        ,
	//wrongPC_br
	output                         wrongPC_br    ,
	output                         wrong_req_r   ,
	output                         br_bus_r_valid,
    //to fs
    output                         ps_to_fs_valid,
    output [`PS_TO_FS_BUS_WD -1:0] ps_to_fs_bus  ,
	input  [`WS_TO_PS_BUS_WD -1:0] ws_to_ps_bus  ,
    // inst sram interface
    output        inst_sram_en   ,
    output [ 3:0] inst_sram_we   ,
    output [31:0] inst_sram_addr ,
    output [31:0] inst_sram_wdata,
	input         addr_ok        ,
	input         data_ok        ,
	//with tlb
	input  [`TLB_TO_PS_BUS_WD -1:0] tlb_to_ps_bus,
	//refetch
	input                           refetch      ,
	input  [                  31:0] refetch_pc   ,
	//to top
	output                          ps_inst_req  ,
	//cacop
	input                           fs_is_icacop ,
	input                           ds_is_icacop ,
	input                           es_is_icacop
);

wire        ps_ready_go;

wire [31:0] seq_pc;
wire [31:0] nextpc;
reg  [31:0] ps_pc;

wire ws_ex, ws_ertn, ws_ex_true, ws_ertn_true;
reg  ws_ex_r, refill_ex_r;
reg [32:0] ws_ertn_r;
assign ws_ex_true = ws_ex || ws_ex_r;
assign ws_ertn_true = ws_ertn || ws_ertn_r[32];
wire [31:0] ex_entrance;
wire [31:0] era;
wire        csr_tlbrera_istlbr;
wire [29:0] csr_tlbrera_pc;
wire        refill_ex;
wire [19:0] ppn;
reg  [19:0] ppn_r;
assign {refill_ex, ppn, csr_tlbrera_istlbr, csr_tlbrera_pc, ex_entrance, ws_ex, ws_ertn, era} = ws_to_ps_bus;
wire         br_taken, br_taken_r;
wire [31:0] br_target;
wire [31:0] br_target_r;
reg [`BR_BUS_WD-1:0] br_bus_r;
reg br_bus_r_valid;
assign {br_taken,br_target} = br_bus;
assign {br_taken_r, br_target_r} = br_bus_r;
reg wrongPC_br, wrong_req_r;
reg refetch_r;

reg inst_sram_en_r;
reg should_keep;

//wrongPC_br：id_stage置br_taken有效，但此前已经发出错误指令的读请求

reg [31:0] nextpc_r;

wire ps_ex;
wire [5:0] ps_ecode;
wire adef_ex, s0_refill_ex, s0_page_inv_ex, s0_ppi_ex;
wire s0_ex;
assign s0_ex = s0_page_inv_ex || s0_ppi_ex;
assign adef_ex = nextpc[1] || nextpc[0];
assign {s0_ppi_ex,
		s0_page_inv_ex,
		s0_refill_ex} = tlb_to_ps_bus;
assign ps_ex = (adef_ex || s0_page_inv_ex || s0_ppi_ex) && !refetch;
assign ps_ecode = adef_ex ? 6'h08 : 
				  s0_page_inv_ex ? 6'h03 : 
				  s0_ppi_ex ? 6'h07 :
				  6'h00;

assign ps_to_fs_bus = {s0_ex,
					   s0_refill_ex,
					   ps_ecode,
					   ps_ex,
					   nextpc
					   };
					   
reg ex_ertn_wrong_pc;

assign ps_to_fs_valid  = ps_ready_go && !ex_ertn_wrong_pc && !((ws_ertn || ws_ex) && should_keep) && !refetch; //ps_valid = ~reset
assign seq_pc = ps_pc + 32'h4;
assign nextpc = should_keep                    ? nextpc_r     : 
				refetch_r                      ? refetch_pc   :
				ws_ertn_true                   ? era          : //(csr_tlbrera_istlbr ? {csr_tlbrera_pc, 2'b0} : era) : 
				ws_ex_true                     ? (refill_ex ? {ppn, 12'b0} : refill_ex_r ? {ppn_r, 12'b0} : ex_entrance) :
				br_taken                       ? br_target    : 
				(br_bus_r_valid && br_taken_r) ? br_target_r  : 
				                                 seq_pc;
												 
assign ps_ready_go = inst_sram_en && addr_ok || ps_ex;

always @(posedge clk) begin
    if (reset) begin
        ps_pc            <= 32'h1bfffffc ;  //**** trick: to make nextpc be 0x1c000000 during reset 
		br_bus_r         <= `BR_BUS_WD'b0;
		br_bus_r_valid   <= 1'b0         ;
		nextpc_r         <= 32'b0        ;
		should_keep      <= 1'b0         ;
		wrongPC_br       <= 1'b0         ;
		ws_ex_r          <= 1'b0         ;
		ws_ertn_r        <= 33'b0        ;
		ex_ertn_wrong_pc <= 1'b0         ;
		wrong_req_r      <= 1'b0         ;
		refetch_r        <= 1'b0         ;
		inst_sram_en_r   <= 1'b0         ;
    end
    else begin
		if (ps_to_fs_valid && fs_allowin) begin
			ps_pc <= nextpc;
		end
		
		if (!br_bus_r_valid) begin
			br_bus_r <= br_bus;
		end
		
		if (ws_ex_true) begin
			br_bus_r_valid <= 1'b0;
		end
		else if (!should_keep && br_taken_r && inst_sram_en) begin
			br_bus_r_valid <= 1'b0;
		end
		else if (br_taken) begin
			if (!inst_sram_en) begin
				br_bus_r_valid <= 1'b1;
			end
/* 			else if (!should_keep && ps_ready_go && fs_allowin) begin //已在该周期内发出正确的请求
				br_bus_r_valid <= 1'b0;
			end
			else if (!should_keep && !ps_ready_go) begin //错误的请求还未发出，br_bus直接抢占nextpc
				br_bus_r_valid <= 1'b0;
			end */
			else if (!should_keep && (fs_allowin || !ps_ready_go)) begin
				br_bus_r_valid <= 1'b0;
			end
			else begin
				br_bus_r_valid <= 1'b1;
			end
		end
		
		if ((ws_ertn || ws_ex) && should_keep) begin
			wrong_req_r <= 1'b1;
		end
		else if (data_ok) begin
			wrong_req_r <= 1'b0;
		end
		
		if (ws_ex_true) begin
			wrongPC_br <= 1'b0;
		end
		else if (data_ok) begin
			wrongPC_br <= 1'b0;
		end
		else if (br_taken && (should_keep || !fs_allowin && ps_ready_go) ) begin
			wrongPC_br <= 1'b1;
		end
/* (!should_keep && ps_ready_go && fs_allowin) || (!should_keep && !ps_ready_go)
!should_keep && (fs_allowin || !ps_ready_go)
(should_keep || !fs_allowin && ps_ready_go) */
		
		if (ws_ex && (ps_ex && !fs_allowin || should_keep || !ps_ready_go)) begin
			ws_ex_r <= 1'b1;
		end
		else if (ps_ready_go && !ex_ertn_wrong_pc) begin
			ws_ex_r <= 1'b0;
		end
		
		if (refill_ex && (ps_ex && !fs_allowin || should_keep || !ps_ready_go)) begin
			refill_ex_r <= 1'b1;
			ppn_r       <=  ppn;
		end
		else if (ps_ready_go && !ex_ertn_wrong_pc) begin
			refill_ex_r <= 1'b0;
		end
		
		if (ws_ertn && (ps_ex && !fs_allowin || should_keep || !ps_ready_go)) begin
			ws_ertn_r <= {ws_ertn, era};
		end
		else if (ps_ready_go && !ex_ertn_wrong_pc) begin
			ws_ertn_r <= 33'b0;
		end

		if (refetch) begin
			refetch_r <= 1'b1;
		end
		else if (ps_ex) begin
			refetch_r <= 1'b0;
		end
		else if (!refetch && !should_keep && inst_sram_en) begin
			refetch_r <= 1'b0;
		end
		
		if (addr_ok) begin
			ex_ertn_wrong_pc <= 1'b0;
		end
		else if ((ws_ertn || ws_ex) && should_keep) begin
			ex_ertn_wrong_pc <= 1'b1;
		end
		
		should_keep    <= inst_sram_en && !addr_ok;
		nextpc_r       <= nextpc                  ;
		inst_sram_en_r <= inst_sram_en            ;
	end
end

assign inst_sram_en    = should_keep ? inst_sram_en_r : (~reset && fs_allowin && !ps_ex && !refetch && !(fs_is_icacop || ds_is_icacop || es_is_icacop));
assign inst_sram_we    = 4'h0;
assign inst_sram_addr  = nextpc;
assign inst_sram_wdata = 32'b0;

assign ps_inst_req = should_keep ? inst_sram_en_r : (~reset && fs_allowin && !adef_ex && !refetch && !(fs_is_icacop || ds_is_icacop || es_is_icacop)); //请求意图，可能会有TLB例外，不是最终的请求信号

endmodule
