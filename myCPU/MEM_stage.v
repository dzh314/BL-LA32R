`include "mycpu.h"

module mem_stage(
    input                          clk           ,
    input                          reset         ,
    //allowin
    input                          ws_allowin    ,
    output                         ms_allowin    ,
	//from rs
	input                          rs_to_ms_valid,
	input  [`RS_TO_MS_BUS_WD -1:0] rs_to_ms_bus  ,
    //to ws
    output                         ms_to_ws_valid,
    output [`MS_TO_WS_BUS_WD -1:0] ms_to_ws_bus  ,
    //from data-sram
    input  [31                 :0] data_sram_rdata,
	input                          data_ok        ,

	output [`MS_TO_DS_BUS_WD -1:0] ms_to_ds_bus   ,
	output                         ms_to_rs_bus   ,
	input  [1                  :0] ws_to_ms_bus   ,
	//refetch
	output                         ms_refetch     
);
wire [31:0] ms_inst;
reg         ms_valid;
wire        ms_ready_go;
wire ws_is_ertn, ws_ex;
assign {ws_is_ertn, ws_ex} = ws_to_ms_bus;

reg [`RS_TO_MS_BUS_WD -1:0] rs_to_ms_bus_r;

wire        ms_refetch_help ;
wire        is_load_or_store;
wire        ms_is_ld        ;
wire		ms_is_ld_b      ;
wire		ms_is_ld_h      ;
wire		ms_is_ld_bu     ;
wire		ms_is_ld_hu     ;
wire		ms_is_csrrd     ;
wire		ms_is_csrwr     ;
wire		ms_is_csrxchg   ;
wire		ms_is_ertn      ;
wire		ms_is_rdtimeh_w ;
wire		ms_is_rdtimel_w ;
wire		ms_is_tlbsrch   ;
wire		ms_is_tlbrd     ;
wire		ms_is_tlbwr     ;
wire		ms_is_tlbfill   ;
wire		ms_is_invtlb    ;
wire [31:0] mask            ;
wire [13:0] csr             ;
wire [ 1:0] addr            ;
wire        ms_is_ld_w      ;
wire        ms_gr_we        ;
wire [ 4:0] ms_dest         ;
wire [31:0] ms_alu_result   ;
wire [31:0] ms_pc           ;
wire		ms_ex           ;
wire        s1_refill_ex    ;
wire        s1_ex           ;
wire        s0_ex           ;
wire        s0_refill_ex    ;
wire        rs_ex           ;
wire [ 5:0] rs_ecode        ;
wire [ 5:0] ms_ecode        ;

wire [ 4:0] invtlb_op       ;
wire [18:0] invtlb_vppn     ;

assign {ms_inst         ,
		ms_is_ld        ,
		is_load_or_store,
		s0_ex           ,
		s0_refill_ex    ,
		s1_ex           ,
		s1_refill_ex    ,
		ms_refetch_help ,
		invtlb_vppn     ,
		invtlb_op       ,
		ms_is_tlbsrch   ,
		ms_is_tlbrd     ,
		ms_is_tlbwr     ,
		ms_is_tlbfill   ,
		ms_is_invtlb    ,
		ms_is_rdtimeh_w ,
		ms_is_rdtimel_w ,
		rs_ecode        ,
		rs_ex           ,
		ms_is_ertn      ,
		mask            ,
		ms_is_csrxchg   ,
		ms_is_csrwr     ,
		ms_is_csrrd     ,
		csr             ,
		addr            ,
		ms_is_ld_hu     ,
		ms_is_ld_bu     ,
		ms_is_ld_h      ,
		ms_is_ld_b      ,
		ms_is_ld_w      ,
		ms_gr_we        ,
		ms_dest         ,
		ms_alu_result   ,
		ms_pc
		} = rs_to_ms_bus_r;

wire [31:0] mem_result;
reg  [31:0] data_sram_rdata_r;

wire ws_gr_we;
assign ws_gr_we = ms_gr_we && !ws_is_ertn;

assign ms_refetch = ms_refetch_help && ms_valid;
					  
assign ms_to_ws_bus = {ms_inst        , //233:202
					   addr           , //201:200
					   mem_result     , //199:168
					   ms_is_ld_b     , //167
					   ms_is_ld_h     , //166
					   ms_is_ld_bu    , //165
					   ms_is_ld_hu    , //164
					   ms_is_ld_w     , //163
					   s0_ex          , //162
					   s0_refill_ex   , //161
					   s1_ex          , //160
					   s1_refill_ex   , //159
					   ms_refetch     , //158
					   invtlb_vppn    , //157:139
					   invtlb_op      , //138:134
					   ms_is_tlbsrch  , //133
					   ms_is_tlbrd    ,
					   ms_is_tlbwr    ,
					   ms_is_tlbfill  ,
					   ms_is_invtlb   ,
					   ms_is_rdtimeh_w, //128
					   ms_is_rdtimel_w, //127
					   ms_ecode       , //126:121
					   ms_ex          , //120
					   ms_is_ertn     , //119
					   mask           , //118:87
					   ms_is_csrxchg  , //86
					   ms_is_csrwr    , //85
					   ms_is_csrrd    , //84
					   csr            , //83:70
					   ws_gr_we       , //69:69
					   ms_dest        , //68:64
					   ms_alu_result  , //63:32
					   ms_pc            // 31:0
					  };

//exception
assign ms_ex = (rs_ex || s1_ex) && !ws_is_ertn && ms_valid;
assign ms_ecode = rs_ecode;

assign ms_ready_go    = !is_load_or_store || data_ok || ms_ex || s1_refill_ex;
assign ms_allowin     = !ms_valid || ms_ready_go && ws_allowin;
assign ms_to_ws_valid = ms_valid && ms_ready_go;
always @(posedge clk) begin
    if (reset) begin
        ms_valid <= 1'b0;
		rs_to_ms_bus_r <= `RS_TO_MS_BUS_WD'b0;
		data_sram_rdata_r <= 32'b0;
    end
    else begin
		if (ws_ex || ws_is_ertn) begin
			ms_valid <= 1'b0;
			end
		else if (ms_allowin) begin
			ms_valid <= rs_to_ms_valid;
		end

		if (rs_to_ms_valid && ms_allowin) begin
			rs_to_ms_bus_r <= (ws_is_ertn || ws_ex) ? `RS_TO_MS_BUS_WD'b0 : rs_to_ms_bus;
		end
		
		if (data_ok) begin
			data_sram_rdata_r <= data_sram_rdata;
		end
	end
end

assign mem_result = data_ok ? data_sram_rdata : data_sram_rdata_r;

assign ms_to_rs_bus = ms_ex | ms_is_ertn;

wire ms_res_from_csr;
assign ms_res_from_csr = ms_is_csrrd | ms_is_csrwr | ms_is_csrxchg;

wire ms_res_from_rdtime = ms_is_rdtimeh_w | ms_is_rdtimel_w;

wire ms_tlb_stall;
assign ms_tlb_stall = (ms_is_tlbsrch || ms_is_invtlb || ms_is_tlbfill) && ms_valid;

wire ms_special_res;
assign ms_special_res = ms_res_from_csr | ms_is_ld;

assign ms_to_ds_bus = {ms_res_from_rdtime,
					   ms_tlb_stall, 
					   ms_special_res,
					   ms_valid,
					   ms_gr_we,
					   ms_dest,
					   ms_alu_result
					  };

endmodule