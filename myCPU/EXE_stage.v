`include "mycpu.h"

module exe_stage(
    input                          clk            ,
    input                          reset          ,
    //allowin
    input                          ms_allowin     ,
    output                         es_allowin     ,
    //from ds
    input                          ds_to_es_valid ,
    input  [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus   ,
    //to ms
    output                         es_to_ms_valid ,
    output [`ES_TO_MS_BUS_WD -1:0] es_to_ms_bus   ,
    // data sram interface
    output                         data_sram_en   ,
    output [                  3:0] data_sram_we   ,
    output [                 31:0] data_sram_addr ,
    output [                 31:0] data_sram_wdata,
	input                          addr_ok        ,
	input                          data_ok        ,
	output [`ES_TO_DS_BUS_WD -1:0] es_to_ds_bus   ,
	input  [                  1:0] ws_to_es_bus   ,
	input                          ms_to_es_bus   ,
	//with tlb
	input  [`TLB_TO_ES_BUS_WD-1:0] tlb_to_es_bus  ,
	//refetch
	output                         es_refetch     ,
	//to top
	output                         es_data_req    ,
	//cacop
	output                         es_is_icacop   ,
	output                         es_is_cacop    ,
	output [                  4:0] cacop_code
);

reg es_valid;
wire es_ready_go;
reg data_sram_en_r;
reg [3:0] data_sram_we_r;
reg [31:0] data_sram_wdata_r;

wire es_is_icacop_h;
wire es_is_cacop_h;
assign es_is_icacop = es_is_icacop_h && es_valid;
assign es_is_cacop  = es_is_cacop_h  && es_valid;

wire ms_ex_or_ertn;
assign ms_ex_or_ertn = ms_to_es_bus;

wire ws_ertn, ws_ex;
assign {ws_ertn, ws_ex} = ws_to_es_bus;

wire es_refetch_help;

reg en_r; //当收到addr_ok信号后，停止发送读请求

reg  [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus_r;
wire		es_mem_we;
wire		es_is_ld_w;
wire		es_is_ld_b;
wire		es_is_ld_h;
wire		es_is_ld_bu;
wire		es_is_ld_hu;
wire		es_is_st_b;
wire		es_is_st_h;
wire		es_is_csrrd;
wire		es_is_csrxchg;
wire		es_is_ertn;
wire		es_is_csrwr;
wire		es_is_rdtimeh_w;
wire		es_is_rdtimel_w;
wire		es_is_tlbsrch;
wire		es_is_tlbrd;
wire		es_is_tlbwr;
wire		es_is_tlbfill;
wire		es_is_invtlb;
wire [13:0] csr;
wire [18:0] es_alu_op;
wire		es_res_from_mem;
wire		es_gr_we;
wire		src2_is_imm;
wire		src1_is_pc;
wire [ 4:0] es_dest;
wire [31:0] es_rj_value;
wire [31:0] es_rkd_value;
wire [31:0] es_pc;
wire [31:0] es_imm;
wire [31:0] es_final_result;
wire        es_is_ld;
wire s1_refill_ex, s1_page_inv_ex, s1_ppi_ex, s0_ex, s0_refill_ex, s1_pme_ex, s1_ex;
reg s1_pme_ex_r;
assign s1_ex = s1_page_inv_ex || s1_ppi_ex || (s1_pme_ex && es_mem_we || s1_pme_ex_r); //除了重填例外的其它TLB例外，因为重填例外需要特别处理，所以独立成s1_refill_ex

wire res_from_csr;
assign res_from_csr = es_is_csrrd | es_is_csrwr | es_is_csrxchg;

wire res_from_rdtime;
assign res_from_rdtime = es_is_rdtimeh_w | es_is_rdtimel_w;

wire is_load_or_store;
assign is_load_or_store = es_mem_we | es_is_ld_w | es_is_ld_b | es_is_ld_h | es_is_ld_bu | es_is_ld_hu;

//div_w
wire is_mul_w;
wire is_mulh_w;
wire is_mulh_wu;
wire is_div_w;
wire is_div_wu;
wire is_mod_w;
wire is_mod_wu;
wire is_div, is_divu;
assign is_div = is_div_w || is_mod_w;
assign is_divu = is_div_wu || is_mod_wu;
wire [63:0] unsigned_prod, signed_prod;
wire [63:0] div_w_res, div_wu_res;
wire  div_divisor_tready,  div_dividend_tready;
wire divu_divisor_tready, divu_dividend_tready;
wire div_divisor_tvalid,  div_dividend_tvalid, divu_divisor_tvalid, divu_dividend_tvalid;
wire div_w_ready, div_wu_ready;
assign is_div_w  = es_alu_op[12];
assign is_div_wu = es_alu_op[13];
assign is_mul_w  = es_alu_op[14];
assign is_mulh_w = es_alu_op[15];
assign is_mulh_wu= es_alu_op[16];
assign is_mod_w  = es_alu_op[17];
assign is_mod_wu = es_alu_op[18];
assign unsigned_prod = es_rj_value * es_rkd_value;
assign signed_prod   = $signed(es_rj_value) * $signed(es_rkd_value);
reg div_count, divu_count;
always @(posedge clk) begin
	if(reset) begin
	div_count <= 1'b1;
	end
	else if(is_div && div_divisor_tvalid) begin
	div_count <= 1'b0;
	end
	else if(div_w_ready || !is_div)
	div_count <= 1'b1;
	else div_count <= div_count;
end
always @(posedge clk) begin
	if(reset) begin
	divu_count <= 1'b1;
	end
	else if(is_divu && divu_divisor_tvalid) begin
	divu_count <= 1'b0;
	end
	else if(div_wu_ready || !is_divu)
	divu_count <= 1'b1;
	else divu_count	<= divu_count;
end
assign div_divisor_tvalid   = is_div & div_divisor_tready  & div_count && es_valid;
assign div_dividend_tvalid  = is_div & div_dividend_tready & div_count && es_valid;
assign divu_divisor_tvalid  = is_divu& divu_divisor_tready & divu_count && es_valid;
assign divu_dividend_tvalid = is_divu& divu_dividend_tready& divu_count && es_valid;
mydiv mydiv(
	.aclk				   (clk		           ),
	
	.s_axis_divisor_tdata  (es_rkd_value       ),
	.s_axis_divisor_tready (div_divisor_tready ),
	.s_axis_divisor_tvalid (div_divisor_tvalid ),
	
	.s_axis_dividend_tdata (es_rj_value        ),
	.s_axis_dividend_tready(div_dividend_tready),
	.s_axis_dividend_tvalid(div_dividend_tvalid),
	
	.m_axis_dout_tdata     (div_w_res          ),
	.m_axis_dout_tvalid    (div_w_ready        )
);
mydivu mydivu(
	.aclk				   (clk		            ),
	
	.s_axis_divisor_tdata  (es_rkd_value        ),
	.s_axis_divisor_tready (divu_divisor_tready ),
	.s_axis_divisor_tvalid (divu_divisor_tvalid ),
	
	.s_axis_dividend_tdata (es_rj_value         ),
	.s_axis_dividend_tready(divu_dividend_tready),
	.s_axis_dividend_tvalid(divu_dividend_tvalid),
	
	.m_axis_dout_tdata     (div_wu_res          ),
	.m_axis_dout_tvalid    (div_wu_ready        )
);

wire ds_ex, es_ex;
wire [5:0] ds_ecode, es_ecode;

wire [ 4:0] invtlb_op;
wire [18:0] invtlb_vppn;
assign invtlb_vppn = es_rkd_value[31:13];
	   
assign {es_is_cacop_h  ,
		es_is_icacop_h ,
		s0_ex          ,
		s0_refill_ex   ,
		es_refetch_help,
		invtlb_op      ,
		es_is_tlbsrch  ,
		es_is_tlbrd    ,
		es_is_tlbwr    ,
		es_is_tlbfill  ,
		es_is_invtlb   ,
		es_is_rdtimeh_w,
		es_is_rdtimel_w,
		ds_ecode       ,
		ds_ex          ,
		es_is_ertn     ,
		es_is_csrxchg  ,
		es_is_csrwr    ,
		es_is_csrrd    ,
		csr            ,
		es_is_st_h     ,
		es_is_st_b     ,
		es_is_ld_hu    ,
		es_is_ld_bu    ,
		es_is_ld_h     ,
		es_is_ld_b     ,
		es_is_ld_w     ,
		es_imm         ,
		src2_is_imm    ,
		src1_is_pc     ,
		es_alu_op      ,
		es_res_from_mem,
		es_gr_we       ,
		es_mem_we      ,
		es_dest        ,
		es_rj_value    ,
		es_rkd_value   ,
		es_pc
		} = ds_to_es_bus_r;

wire [31:0] es_alu_src1   ;
wire [31:0] es_alu_src2   ;
wire [31:0] es_alu_result ;

wire special_res;
assign special_res = is_div_w | is_div_wu | is_mul_w | is_mulh_w | is_mulh_wu | is_mod_w | is_mod_wu;

assign es_final_result = special_res ?
						({32{is_div_w}}  & div_w_res[63:32]    |
						 {32{is_div_wu}} & div_wu_res[63:32]   | 
						 {32{is_mul_w}}  & signed_prod[31:0]   |  
						 {32{is_mulh_w}} & signed_prod[63:32]  | 
						 {32{is_mulh_wu}}& unsigned_prod[63:32]|  
						 {32{is_mod_w}}  & div_w_res[31:0]     | 
						 {32{is_mod_wu}} & div_wu_res[31:0]     ) : 
						 es_alu_result;
					  
/* assign es_final_result = is_div_w  ? div_w_res[63:32]     : 
						 is_div_wu ? div_wu_res[63:32]    : 
						 is_mul_w  ? signed_prod[31:0]    : 
						 is_mulh_w ? signed_prod[63:32]   :
						 is_mulh_wu? unsigned_prod[63:32] : 
						 is_mod_w  ? div_w_res[31:0]      :
						 is_mod_wu ? div_wu_res[31:0]     :
						 es_alu_result; */
						 
assign es_refetch = es_refetch_help && es_valid;
						 
assign es_to_ms_bus = {s0_ex              , //170
					   s0_refill_ex       , //169
					   s1_ex              , //168
					   s1_refill_ex       , //167
					   es_refetch         , //166
					   invtlb_vppn        , //165:147
					   invtlb_op          , //146:142
					   es_is_tlbsrch      , //141
					   es_is_tlbrd        ,
					   es_is_tlbwr        ,
					   es_is_tlbfill      ,
					   es_is_invtlb       , //137
					   is_load_or_store   , //136
					   es_is_rdtimeh_w    , //135
					   es_is_rdtimel_w    , //134
					   es_ecode           , //133:128
					   es_ex              , //127
					   es_is_ertn         , //126
					   es_rj_value        , //125:94
					   es_is_csrxchg      , //93
					   es_is_csrwr        , //92
					   es_is_csrrd        , //91
					   csr                , //90:77
					   data_sram_addr[1:0], //76:75
					   es_is_ld_hu        , //74
					   es_is_ld_bu        , //73
					   es_is_ld_h         , //72
					   es_is_ld_b         , //71:71
					   es_res_from_mem    , //70:70 其实是es_is_ld_w
					   es_gr_we           , //69:69
					   es_dest            , //68:64
					   es_final_result    , //63:32
					   es_pc //31:0
};

// exception
wire ale_ex, ld_w_ex, ld_h_ex, ld_hu_ex, st_h_ex, st_w_ex;
assign ld_w_ex = es_is_ld_w && (es_alu_result[1] || es_alu_result[0]);
assign ld_h_ex = es_is_ld_h && es_alu_result[0];
assign ld_hu_ex = es_is_ld_hu && es_alu_result[0];
assign st_h_ex = es_is_st_h && es_alu_result[0];
assign st_w_ex = es_mem_we && !es_is_st_b && !es_is_st_h && (es_alu_result[1] || es_alu_result[0]);
assign ale_ex = ld_w_ex | ld_h_ex | ld_hu_ex | st_h_ex | st_w_ex;

// tlb_ex
assign {s1_pme_ex,
		s1_ppi_ex,
		s1_page_inv_ex,
		s1_refill_ex} = tlb_to_es_bus;

assign es_ex = (ale_ex || ds_ex || s1_ex) && !ws_ertn && es_valid;
assign es_ecode = ds_ex ? ds_ecode : 
				  ale_ex ? 6'h09 : 
				  s1_page_inv_ex ? (es_mem_we ? 6'h2 : 6'h1) :
				  s1_ppi_ex ? 6'h07 :
				  (s1_pme_ex && es_mem_we || s1_pme_ex_r) ? 6'h04 : //pme例外放在mem_stage报出，ecode放在exe_stage修改，同时s1_ex是包含pme_ex的
				  6'h00;

assign es_ready_go = (!(is_div || is_divu || data_sram_en) || is_div && div_w_ready || is_divu && div_wu_ready) 
				  && (!is_load_or_store || data_ok || en_r) && (!es_is_cacop || addr_ok)
				  || es_ex || s1_refill_ex; 
//因为en_r在data_ok后才会为1，所以其实最外层&&后的逻辑是，如果es阶段发出读写请求，但是mem阶段还没有完成，es阶段实际上并不是ready_go的，如果ready_go了那么进来的指令很有可能出问题|| is_load_or_store && (!en_r || data_sram_en&&addr_ok);
assign es_allowin     = !es_valid || es_ready_go && ms_allowin;
assign es_to_ms_valid =  es_valid && es_ready_go;

always @(posedge clk) begin
    if (reset) begin
        es_valid <= 1'b0;
		ds_to_es_bus_r <= `DS_TO_ES_BUS_WD'b0;
		en_r <= 1'b1;
		s1_pme_ex_r <= 1'b0;
    end
    else begin
		if (ws_ex || ws_ertn) begin
			es_valid <= 1'b0;
		end
		else if (es_allowin) begin
			es_valid <= ds_to_es_valid;
		end
		if (ds_to_es_valid && es_allowin) begin
			ds_to_es_bus_r <= (ws_ertn || ws_ex) ? `DS_TO_ES_BUS_WD'b0 : ds_to_es_bus;
		end
		
		if (data_ok) begin
			en_r <= 1'b1;
		end
		else if (data_sram_en && addr_ok) begin
			en_r <= 1'b0;
		end
		
		if (s1_pme_ex && es_mem_we) begin
			s1_pme_ex_r <= 1'b1;
		end
		else if (ds_to_es_valid && es_allowin) begin
			s1_pme_ex_r <= 1'b0;
		end
	end
end
									  
assign es_alu_src1 = (es_is_csrwr | es_is_csrxchg) ? 32'b0 : src1_is_pc  ? es_pc[31:0] : es_rj_value;
assign es_alu_src2 = src2_is_imm ? es_imm : es_rkd_value;

alu u_alu(
    .alu_op     (es_alu_op[11:0]),
    .alu_src1   (es_alu_src1    ),
    .alu_src2   (es_alu_src2    ),
    .alu_result (es_alu_result  )
    );

//cacop
assign cacop_code = invtlb_op;

//load/store
wire [3:0] we_help;
wire [3:0] we_st_b_help;
wire [3:0] we_st_h_help;
wire [31:0] wdata_st_b_help;
wire [31:0] wdata_st_h_help;
assign we_st_b_help = (es_alu_result[1:0] == 2'b11) ? 4'b1000 : (es_alu_result[1:0] == 2'b10) ? 4'b0100 : (es_alu_result[1:0] == 2'b01) ? 4'b0010 : 4'b0001;
assign we_st_h_help = (es_alu_result[1:0] == 2'b10) ? 4'b1100 : 4'b0011;
assign we_help = es_is_st_b ? we_st_b_help : 
				 es_is_st_h ? we_st_h_help : 4'b1111;
assign wdata_st_b_help = {4{es_rkd_value[7:0]}};
assign wdata_st_h_help = {2{es_rkd_value[15:0]}};
assign data_sram_en = es_valid && is_load_or_store && en_r && !es_ex && !s1_refill_ex; 
assign data_sram_we = (ms_ex_or_ertn || ws_ex || es_ex || s1_refill_ex) ? 4'b0 :
					  (es_mem_we && es_valid)           ? we_help : 4'b0;
assign data_sram_addr = es_alu_result;
/* assign data_sram_size = (es_is_ld_b || es_is_ld_bu || es_is_st_b) ? 2'b00 : 
						(es_is_ld_h || es_is_ld_hu || es_is_st_h) ? 2'b01 :
						2'b10; */
assign data_sram_wdata = es_is_st_b ? wdata_st_b_help :
						 es_is_st_h ? wdata_st_h_help :
						 es_rkd_value;
						 
assign es_data_req = es_valid && is_load_or_store && en_r;
						 
wire es_tlb_stall;
assign es_tlb_stall = (es_is_tlbsrch || es_is_invtlb || es_is_tlbfill) && es_valid;

assign es_is_ld = es_is_ld_hu || es_is_ld_bu || es_is_ld_h || es_is_ld_b || es_is_ld_w;
assign es_to_ds_bus = {es_tlb_stall, res_from_rdtime,res_from_csr, es_is_ld, es_valid, es_gr_we, es_dest, es_alu_result};
					   
endmodule