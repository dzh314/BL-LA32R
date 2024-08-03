`include "mycpu.h"

module pre_mem_stage(
    input                          clk            ,
    input                          reset          ,
    //allowin
    input                          ms_allowin     ,
    output                         rs_allowin     ,
    //from es
    input                          es_to_rs_valid ,
    input  [`ES_TO_RS_BUS_WD -1:0] es_to_rs_bus   ,
    //to ms
    output                         rs_to_ms_valid ,
    output [`RS_TO_MS_BUS_WD -1:0] rs_to_ms_bus   ,
    //from ms
    input                          ms_to_rs_bus   ,
    //inst sram interface
    output                         data_sram_en   ,
    output [                  3:0] data_sram_we   ,
    output [                 31:0] data_sram_addr ,
    output [                 31:0] data_sram_wdata,
	input                          addr_ok        ,
    //to ds
    output [`RS_TO_DS_BUS_WD -1:0] rs_to_ds_bus   ,
    //from ws
    input  [                  1:0] ws_to_rs_bus   ,
    //with tlb
    input  [`TLB_TO_RS_BUS_WD-1:0] tlb_to_rs_bus  ,
    //refetch
    output                         rs_refetch     ,
    //cacop
    output                         rs_is_icacop   ,
    output                         rs_is_cacop    ,
    output [                  4:0] cacop_code     ,
    //to top
    output                         rs_data_req
); /////////////////先不支持cacop
wire [31:0] rs_inst;
reg rs_valid;
wire rs_ready_go;

wire ms_ex_or_ertn;
assign ms_ex_or_ertn = ms_to_rs_bus;

wire ws_ertn, ws_ex;
assign {ws_ertn, ws_ex} = ws_to_rs_bus;

reg [`ES_TO_RS_BUS_WD -1:0] es_to_rs_bus_r;

wire s0_ex;
wire s0_refill_ex;
wire s1_ex;
wire s1_refill_ex;
wire [18:0] invtlb_vppn;
wire [4:0] invtlb_op;
wire rs_is_tlbsrch;
wire rs_is_tlbrd;
wire rs_is_tlbwr;
wire rs_is_tlbfill;
wire rs_is_invtlb;
wire is_load_or_store;
wire rs_is_rdtimeh_w;
wire rs_is_rdtimel_w;
wire rs_is_ertn;
wire [31:0] mask;
wire rs_is_csrxchg;
wire rs_is_csrwr;
wire rs_is_csrrd;
wire [13:0] csr;
wire rs_is_ld_hu;
wire rs_is_ld_bu;
wire rs_is_ld_h;
wire rs_is_ld_b;
wire rs_is_ld_w;
wire rs_is_st_w;
wire rs_is_st_h;
wire rs_is_st_b;
wire rs_is_ld;
wire rs_gr_we;
wire [4:0] rs_dest;
wire [31:0] rs_pc;
wire [31:0] rs_alu_result;
wire [31:0] es_rkd_value;
wire rs_refetch_help;
wire rs_mem_we;
wire es_ex;
wire rs_ex;
wire [5:0] es_ecode;
wire [5:0] rs_ecode;
wire rs_is_icacop_h;
wire rs_is_cacop_h;
wire s1_page_inv_ex;
wire s1_ppi_ex;
wire s1_pme_ex;

always @(posedge clk) begin
    if (reset) begin
        rs_valid <= 1'b0;
        es_to_rs_bus_r <= `ES_TO_RS_BUS_WD'b0;
    end
    else begin
        if (ws_ex || ws_ertn) begin
            rs_valid <= 1'b0;
        end
        else if (rs_allowin) begin
            rs_valid <= es_to_rs_valid;
        end

        if (es_to_rs_valid && rs_allowin) begin
            es_to_rs_bus_r <= (ws_ertn || ws_ex) ? `ES_TO_RS_BUS_WD'b0 : es_to_rs_bus;
        end
    end
end

assign {rs_inst            ,
        is_load_or_store   ,
        rs_is_cacop_h      ,
        rs_is_icacop_h     ,
        rs_is_ld           ,
        rs_mem_we          ,
        rs_is_st_b         ,
        rs_is_st_h         ,
        rs_is_st_w         ,
        es_rkd_value       ,
        s0_ex              ,
        s0_refill_ex       ,
        rs_refetch_help    ,
        invtlb_vppn        ,
        invtlb_op          ,
        rs_is_tlbsrch      ,
        rs_is_tlbrd        ,
        rs_is_tlbwr        ,
        rs_is_tlbfill      ,
        rs_is_invtlb       ,
        rs_is_rdtimeh_w    ,
        rs_is_rdtimel_w    ,
        es_ecode           ,
        es_ex              ,
        rs_is_ertn         ,
        mask               ,
        rs_is_csrxchg      ,
        rs_is_csrwr        ,
        rs_is_csrrd        ,
        csr                ,
        rs_is_ld_hu        ,
        rs_is_ld_bu        ,
        rs_is_ld_h         ,
        rs_is_ld_b         ,
        rs_is_ld_w         ,
        rs_gr_we           ,
        rs_dest            ,
        rs_alu_result      ,
        rs_pc
        } = es_to_rs_bus_r;

assign rs_to_ms_bus = {rs_inst            , //203:172
                       rs_is_ld           , //171
                       is_load_or_store   , //170
                       s0_ex              , //169
					   s0_refill_ex       , //168
					   s1_ex              , //167
					   s1_refill_ex       , //166
					   rs_refetch         , //165
					   invtlb_vppn        , //164:146
					   invtlb_op          , //145:141
					   rs_is_tlbsrch      , //140
					   rs_is_tlbrd        , //139
					   rs_is_tlbwr        , //138
					   rs_is_tlbfill      , //137
					   rs_is_invtlb       , //136
					   rs_is_rdtimeh_w    , //135
					   rs_is_rdtimel_w    , //134
					   rs_ecode           , //133:128
					   rs_ex              , //127
					   rs_is_ertn         , //126
					   mask               , //125:94
					   rs_is_csrxchg      , //93
					   rs_is_csrwr        , //92
					   rs_is_csrrd        , //91
					   csr                , //90:77
					   data_sram_addr[1:0], //76:75
					   rs_is_ld_hu        , //74
					   rs_is_ld_bu        , //73
					   rs_is_ld_h         , //72
					   rs_is_ld_b         , //71
					   rs_is_ld_w         , //70
					   rs_gr_we           , //69
					   rs_dest            , //68:64
					   rs_alu_result      , //63:32
					   rs_pc                //31:0
};

//exception
wire ale_ex, ld_w_ex, ld_h_ex, ld_hu_ex, st_h_ex, st_w_ex;
assign ld_w_ex = rs_is_ld_w && (rs_alu_result[1] || rs_alu_result[0]);
assign ld_h_ex = rs_is_ld_h && rs_alu_result[0];
assign ld_hu_ex = rs_is_ld_hu && rs_alu_result[0];
assign st_h_ex = rs_is_st_h && rs_alu_result[0];
assign st_w_ex = rs_is_st_w && (rs_alu_result[1] || rs_alu_result[0]);
assign ale_ex = ld_w_ex | ld_h_ex | ld_hu_ex | st_h_ex | st_w_ex;

assign {s1_pme_ex,
		s1_ppi_ex,
		s1_page_inv_ex,
		s1_refill_ex} = tlb_to_rs_bus;
assign s1_ex = s1_page_inv_ex || s1_ppi_ex || s1_pme_ex && rs_mem_we; //除了重填例外的其它TLB例外，因为重填例外需要特别处理，所以独立成s1_refill_ex

assign rs_ex = (ale_ex || es_ex || s1_ex) && !ws_ertn && rs_valid;
assign rs_ecode = es_ex ? es_ecode :
                  ale_ex ? 6'h09 :
                  s1_page_inv_ex ? (rs_mem_we ? 6'h2 : 6'h1) :
                  s1_ppi_ex ? 6'h07 :
                  (s1_pme_ex && rs_mem_we) ? 6'h04 :
                  6'h00;

//load/store
wire [3:0] we_help;
wire [3:0] we_st_b_help;
wire [3:0] we_st_h_help;
wire [31:0] wdata_st_b_help;
wire [31:0] wdata_st_h_help;
assign we_st_b_help = (rs_alu_result[1:0] == 2'b11) ? 4'b1000 : (rs_alu_result[1:0] == 2'b10) ? 4'b0100 : (rs_alu_result[1:0] == 2'b01) ? 4'b0010 : 4'b0001;
assign we_st_h_help = (rs_alu_result[1:0] == 2'b10) ? 4'b1100 : 4'b0011;
assign we_help = rs_is_st_b ? we_st_b_help : 
				 rs_is_st_h ? we_st_h_help : 4'b1111;
assign wdata_st_b_help = {4{es_rkd_value[7:0]}};
assign wdata_st_h_help = {2{es_rkd_value[15:0]}};
assign data_sram_en = rs_valid && is_load_or_store && !rs_ex && !s1_refill_ex && !ms_ex_or_ertn; 
assign data_sram_we = (ms_ex_or_ertn || ws_ex || rs_ex || s1_refill_ex) ? 4'b0 :
					  (rs_mem_we && rs_valid)           ? we_help : 4'b0;
assign data_sram_addr = rs_alu_result;

assign data_sram_wdata = rs_is_st_b ? wdata_st_b_help :
						 rs_is_st_h ? wdata_st_h_help :
						 es_rkd_value;
						 
assign rs_data_req = rs_valid && is_load_or_store;

//tlb
assign rs_refetch = rs_refetch_help && rs_valid;

//cacop
assign cacop_code = invtlb_op;
assign rs_is_icacop = rs_is_icacop_h && rs_valid;
assign rs_is_cacop  = rs_is_cacop_h  && rs_valid;


assign rs_ready_go = (!data_sram_en || addr_ok) && (!rs_is_cacop || addr_ok) || rs_ex || s1_refill_ex;
assign rs_allowin = !rs_valid || rs_ready_go && ms_allowin;
assign rs_to_ms_valid = rs_valid && rs_ready_go;

wire rs_tlb_stall;
assign rs_tlb_stall = (rs_is_tlbsrch || rs_is_invtlb || rs_is_tlbfill) && rs_valid;

wire res_from_csr;
assign res_from_csr = rs_is_csrrd | rs_is_csrwr | rs_is_csrxchg;

wire res_from_rdtime;
assign res_from_rdtime = rs_is_rdtimeh_w | rs_is_rdtimel_w;

wire rs_special_res;
assign rs_special_res = res_from_csr | rs_is_ld;

assign rs_to_ds_bus = {res_from_rdtime,
                       rs_tlb_stall,
                       rs_special_res,
                       rs_valid, 
                       rs_gr_we, 
                       rs_dest, 
                       rs_alu_result
};

endmodule