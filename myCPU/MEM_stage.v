`include "mycpu.h"

module mem_stage(
    input                          clk           ,
    input                          reset         ,
    //allowin
    input                          ws_allowin    ,
    output                         ms_allowin    ,
    //from es
    input                          es_to_ms_valid,
    input  [`ES_TO_MS_BUS_WD -1:0] es_to_ms_bus  ,
    //to ws
    output                         ms_to_ws_valid,
    output [`MS_TO_WS_BUS_WD -1:0] ms_to_ws_bus  ,
    //from data-sram
    input  [31                 :0] data_sram_rdata,
	output [`MS_TO_DS_BUS_WD -1:0] ms_to_ds_bus   ,
	output                         ms_to_es_bus   ,
	input  [1                  :0] ws_to_ms_bus   ,
	input                          data_ok        ,
	//refetch
	output                         ms_refetch     
);

reg         ms_valid;
wire        ms_ready_go;
wire ws_is_ertn, ws_ex;
assign {ws_is_ertn, ws_ex} = ws_to_ms_bus;

reg [`ES_TO_MS_BUS_WD -1:0] es_to_ms_bus_r;

wire ms_refetch_help;
wire		ms_is_ld_b     ;
wire		ms_is_ld_h     ;
wire		ms_is_ld_bu    ;
wire		ms_is_ld_hu    ;
wire		ms_is_csrrd    ;
wire		ms_is_csrwr    ;
wire		ms_is_csrxchg  ;
wire		ms_is_ertn     ;
wire		ms_is_rdtimeh_w;
wire		ms_is_rdtimel_w;
wire		ms_is_tlbsrch  ;
wire		ms_is_tlbrd    ;
wire		ms_is_tlbwr    ;
wire		ms_is_tlbfill  ;
wire		ms_is_invtlb   ;
wire [31:0] mask           ;
wire [13:0] csr            ;
wire [ 1:0] addr           ;
wire        ms_res_from_mem;
wire        ms_gr_we       ;
wire [ 4:0] ms_dest        ;
wire [31:0] ms_alu_result  ;
wire [31:0] ms_pc          ;
wire		ms_ex          ;
wire        s1_refill_ex   ;
wire        s1_ex          ;
wire        s0_ex          ;
wire        s0_refill_ex   ;
wire        es_ex          ;
wire [ 5:0] es_ecode       ;
wire [ 5:0] ms_ecode       ;
wire        is_load_or_store;

wire [ 4:0] invtlb_op;
wire [18:0] invtlb_vppn;

assign {s0_ex           ,
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
		is_load_or_store,
		ms_is_rdtimeh_w ,
		ms_is_rdtimel_w ,
		es_ecode        ,
		es_ex           ,
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
		ms_res_from_mem ,
		ms_gr_we        ,
		ms_dest         ,
		ms_alu_result   ,
		ms_pc
		} = es_to_ms_bus_r;

wire [31:0] mem_result;
wire [31:0] ms_final_result;
reg  [31:0] data_sram_rdata_r;

wire ws_gr_we;
assign ws_gr_we = ms_gr_we && !ws_is_ertn;

assign ms_refetch = ms_refetch_help && ms_valid;
					  
assign ms_to_ws_bus = {s0_ex          , //162
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
					   ms_final_result, //63:32
					   ms_pc            // 31:0
					  };

//exception
assign ms_ex = (es_ex || s1_ex) && !ws_is_ertn && ms_valid;
assign ms_ecode = es_ecode;

assign ms_ready_go    = 1'b1;
assign ms_allowin     = !ms_valid || ms_ready_go && ws_allowin;
assign ms_to_ws_valid = ms_valid && ms_ready_go;
always @(posedge clk) begin
    if (reset) begin
        ms_valid <= 1'b0;
		es_to_ms_bus_r <= `ES_TO_MS_BUS_WD'b0;
		data_sram_rdata_r <= 32'b0;
    end
    else begin
		if (ws_ex || ws_is_ertn) begin
			ms_valid <= 1'b0;
			end
		else if (ms_allowin) begin
			ms_valid <= es_to_ms_valid;
		end

		if (es_to_ms_valid && ms_allowin) begin
			es_to_ms_bus_r <= (ws_is_ertn || ws_ex) ? `ES_TO_MS_BUS_WD'b0 : es_to_ms_bus;
		end
		
		if (data_ok) begin
			data_sram_rdata_r <= data_sram_rdata;
		end
	end
end

assign mem_result = data_sram_rdata_r;

wire [ 7:0] ld_b_help;
wire [15:0] ld_h_help;
assign ld_b_help = (addr == 2'b11) ? mem_result[31:24] : (addr == 2'b10) ? mem_result[23:16] : (addr == 2'b01) ? mem_result[15:8] : mem_result[7:0];
assign ld_h_help = (addr == 2'b10) ? mem_result[31:16] : mem_result[15:0];

assign ms_final_result = (ms_ex || s1_refill_ex) ? ms_alu_result                    :
						 ms_is_ld_b              ? {{24{ld_b_help[7]}}, ld_b_help}  : 
						 ms_is_ld_h              ? {{16{ld_h_help[15]}}, ld_h_help} :
						 ms_is_ld_bu             ? {24'b0, ld_b_help}               : 
						 ms_is_ld_hu             ? {16'b0, ld_h_help}               :
						 ms_res_from_mem         ? mem_result                       : ms_alu_result;

assign ms_to_es_bus = ms_ex | ms_is_ertn;

wire ms_res_from_csr;
assign ms_res_from_csr = ms_is_csrrd | ms_is_csrwr | ms_is_csrxchg;

wire ms_res_from_rdtime = ms_is_rdtimeh_w | ms_is_rdtimel_w;

wire ms_tlb_stall;
assign ms_tlb_stall = (ms_is_tlbsrch || ms_is_invtlb || ms_is_tlbfill) && ms_valid;

assign ms_to_ds_bus = {ms_tlb_stall, 
					   ms_res_from_rdtime,
					   ms_res_from_csr,
					   ms_valid,
					   ms_gr_we,
					   ms_dest,
					   ms_final_result
					  };

endmodule