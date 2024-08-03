`include "mycpu.h"

module id_stage(
    input                          clk           ,
    input                          reset         ,
    //allowin
    input                          es_allowin    ,
    output                         ds_allowin    ,
    //from fs
    input                          fs_to_ds_valid,
    input  [`FS_TO_DS_BUS_WD -1:0] fs_to_ds_bus  ,
    //to es
    output                         ds_to_es_valid,
    output [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus  ,
    //to rf: for write back
    input  [`WS_TO_RF_BUS_WD -1:0] ws_to_rf_bus  ,
	//input
	input  [`ES_TO_DS_BUS_WD -1:0] es_to_ds_bus  ,
	input  [`RS_TO_DS_BUS_WD -1:0] rs_to_ds_bus  ,
	input  [`MS_TO_DS_BUS_WD -1:0] ms_to_ds_bus  ,
	input  [`WS_TO_DS_BUS_WD -1:0] ws_to_ds_bus  ,
	//refetch
	output                         ds_refetch    ,
	//cacop
	output                         ds_is_icacop  ,
	// from es
	input                          es_br_taken
	`ifdef DIFFTEST_EN
	,
	output [                 31:0] rf_to_diff[31:0]
	`endif
);

reg         ds_valid      ;
wire        ds_ready_go   ;

wire		ds_ex		  ;
wire [5:0]  ds_ecode      ;
wire        fs_ex         ;
wire [5:0]  fs_ecode      ;
wire        s0_ex         ;
wire        s0_refill_ex  ;
wire        ws_ex         ;
wire		ws_is_ertn    ;
wire		is_instr      ;
wire        ds_is_icacop_h;

//stall
wire        br_stall         ; //避免从dcache->mem_stage->id->br_bus->pre-if的超长路径
wire		rkd_stall        ; //发现wb前递后，rkd的生成很慢；尚不清楚rj不受该前递影响的原因
wire        special_res_stall;
wire        tlb_stall        ;
wire        buffer_stall     ;
wire        rdt_stall        ;

wire [`DS_TO_ES_BUS_WD -1:0] ds_buffer_h    ;
reg  [`DS_TO_ES_BUS_WD -1:0] ds_buffer      ;
reg                          ds_buffer_valid;
wire [31                 :0] fs_pc          ;
reg  [`FS_TO_DS_BUS_WD -1:0] fs_to_ds_bus_r ;
reg			                 es_is_lw_r     ;

wire [63:0] op_31_26_d;
wire [15:0] op_25_22_d;
wire [ 3:0] op_21_20_d;
wire [31:0] op_19_15_d;
wire [31:0] op_14_10_d; //rk;
wire [31:0] op_9_5_d  ; //rj;
wire [31:0] op_4_0_d  ; //rd;
wire        csrxchg_h ;

wire [31:0] ds_inst;
wire [31:0] ds_pc  ;

wire        rf_we   ;
wire [ 4:0] rf_waddr;
wire [31:0] rf_wdata;
wire        rdt_req ;
wire [4 :0] rdt_addr;
wire [31:0] rdt_data;

wire        br_taken ;
wire [31:0] br_target;

wire [18:0] alu_op       ;
wire        src1_is_pc   ;
wire        src2_is_imm  ;
wire        dst_is_r1    ;
wire        src_reg_is_rd;
wire        gr_we        ;
wire        mem_we       ;
wire [ 4:0] dest         ;
wire [31:0] imm          ;
wire [31:0] rj_value     ;
wire [31:0] rkd_value    ;
wire [31:0] br_offs      ;
wire [31:0] jirl_offs    ;

wire [ 4:0] rd ;
wire [ 4:0] rj ;
wire [ 4:0] rk ;
wire [11:0] i12;
wire [19:0] i20;
wire [15:0] i16;
wire [25:0] i26;
wire [13:0] csr;

wire        inst_add_w    ;
wire        inst_sub_w    ;
wire        inst_slt      ;
wire        inst_sltu     ;
wire        inst_nor      ;
wire        inst_and      ;
wire        inst_or       ;
wire        inst_xor      ;
wire        inst_slli_w   ;
wire        inst_srli_w   ;
wire        inst_srai_w   ;
wire        inst_addi_w   ;
wire        inst_ld_w     ;
wire        inst_st_w     ;
wire        inst_jirl     ;
wire        inst_b        ;
wire        inst_bl       ;
wire        inst_beq      ;
wire        inst_bne      ;
wire        inst_lu12i_w  ;
wire		inst_pcaddu12i;
wire		inst_slti     ;
wire		inst_sltui    ;
wire		inst_andi     ;
wire		inst_ori      ;
wire		inst_xori     ;
wire		inst_sll_w    ;
wire		inst_sra_w    ;
wire		inst_srl_w    ;
wire		inst_div_w    ;
wire		inst_div_wu   ;
wire		inst_mul_w    ;
wire		inst_mulh_w   ;
wire		inst_mulh_wu  ;
wire		inst_mod_w    ;
wire		inst_mod_wu   ;
wire		inst_blt      ;
wire		inst_bge      ;
wire		inst_bltu     ;
wire		inst_bgeu     ;
wire		inst_ld_b     ;
wire        inst_ld_h     ;
wire		inst_ld_bu    ;
wire		inst_ld_hu    ;
wire		inst_st_b     ;
wire		inst_st_h     ;
wire		inst_csrrd    ;
wire		inst_csrwr    ;
wire		inst_csrxchg  ;
wire		inst_ertn     ;
wire		inst_syscall  ;
wire		inst_break    ;
wire		inst_rdtimel_w;
wire		inst_rdtimeh_w;
wire		inst_tlbsrch  ;
wire		inst_tlbrd    ;
wire		inst_tlbwr    ;
wire		inst_tlbfill  ;
wire		inst_invtlb   ;
wire        inst_cacop    ;

wire        need_ui5;
wire        need_si12;
wire        need_si20;
wire        need_si26;
wire		need_ui12;
wire        src2_is_4;

wire        rj_eq_rd;
wire		rj_lt_rd;
wire		rj_lt_rd_u;

wire [ 4:0] rf_raddr1;
wire [31:0] rf_rdata1;
wire [ 4:0] rf_raddr2;
wire [31:0] rf_rdata2;

wire invtlb_op_valid;
assign invtlb_op_valid = !(rd[4] || rd[3]) && !(rd[2] && rd[1] && rd[0]);

assign fs_pc = fs_to_ds_bus[31:0];
assign ds_is_icacop = ds_is_icacop_h && ds_valid;

assign {csrxchg_h, //286
		op_31_26_d, //285:222
		op_25_22_d, //221:206
		op_21_20_d, //205:202
		op_19_15_d, //201:170
		op_14_10_d, //169:138
		op_9_5_d  , //137:106
		op_4_0_d  , //105:74
		ds_is_icacop_h,
		s0_ex       ,
		s0_refill_ex,
		fs_ecode    ,
		fs_ex       ,
		ds_inst     ,
        ds_pc  
		} = fs_to_ds_bus_r;

assign {rdt_req ,
		rdt_addr,
		rdt_data,
		rf_we   ,  //37:37
        rf_waddr,  //36:32
        rf_wdata   //31:0
       } = ws_to_rf_bus;

assign is_instr = inst_add_w     | inst_sub_w   | inst_slt       | inst_sltu      | inst_nor     | inst_and  | inst_or
				| inst_xor       | inst_slli_w  | inst_srli_w    | inst_srai_w    | inst_addi_w  | inst_ld_w
				| inst_st_w      | inst_jirl    | inst_b         | inst_bl        | inst_beq     | inst_bne  | inst_lu12i_w
				| inst_pcaddu12i | inst_slti    | inst_sltui     | inst_andi      | inst_ori     | inst_xori
				| inst_sll_w     | inst_sra_w   | inst_srl_w     | inst_div_w     | inst_div_wu  | inst_mul_w
				| inst_mulh_w    | inst_mulh_wu | inst_mod_w     | inst_mod_wu    | inst_blt     | inst_bge
				| inst_bltu      | inst_bgeu    | inst_ld_b      | inst_ld_h      | inst_ld_bu   | inst_ld_hu
				| inst_st_b      | inst_st_h    | inst_csrrd     | inst_csrwr     | inst_csrxchg | inst_ertn
				| inst_syscall   | inst_break   | inst_rdtimel_w | inst_rdtimeh_w | inst_tlbsrch | inst_cacop
				| inst_tlbrd     | inst_tlbwr   | inst_tlbfill   | (inst_invtlb && invtlb_op_valid);

assign ds_ex = (fs_ex | inst_syscall | inst_break | (!is_instr && !(ws_ex || ws_is_ertn)) ) && ds_valid;
assign ds_ecode = fs_ex        ? fs_ecode :
				  (!is_instr)  ? 6'h0d    : 
				  inst_syscall ? 6'h0b    : 
				  inst_break   ? 6'h0c    : 
				  6'h00;

assign ds_to_es_bus = ds_buffer_valid ? ds_buffer : ds_buffer_h;

assign ds_buffer_h  = {ds_inst       , //270:239
					   br_taken      , //238
					   br_target     , //237:206
					   inst_cacop    , //205
					   ds_is_icacop  , //204
					   s0_ex         , //203
					   s0_refill_ex  , //202
					   ds_refetch    , //201
					   rd            , //200:196
					   inst_tlbsrch  , //220 //
					   inst_tlbrd    , //219 //
					   inst_tlbwr    , //218 //
					   inst_tlbfill  , //217 //
					   inst_invtlb   , //191
					   inst_rdtimeh_w, //190
					   inst_rdtimel_w, //189
					   ds_ecode      , //188:183
					   ds_ex         , //182
					   inst_ertn     , //181
					   inst_csrxchg  , //180
					   inst_csrwr    , //179
					   inst_csrrd    , //178
					   csr           , //177:164
					   inst_st_h     , //163
					   inst_st_b     , //162
					   inst_ld_hu    , //161 
					   inst_ld_bu    , //160
					   inst_ld_h     , //159
					   inst_ld_b     , //158
					   inst_ld_w     , //157
					   imm           , //156:125
					   src2_is_imm   , //124
					   src1_is_pc    , //123
					   alu_op        , //122:104
					   inst_ld_w     , //103
					   gr_we         , //102
					   mem_we        , //101
					   dest          , //100:96
					   rj_value      , //95:64
					   rkd_value     , //63:32
					   ds_pc           //31:0
};

// forward unit
wire 		es_tlb_stall  , rs_tlb_stall  , ms_tlb_stall  , ws_tlb_stall  ;
wire 	    es_valid      , rs_valid      , ms_valid      , ws_valid      ;
wire	    es_gr_we      , rs_gr_we      , ms_gr_we      , ws_gr_we      ;
wire [4 :0] es_dest       , rs_dest       , ms_dest       , ws_dest       ;
wire [31:0] es_data       , rs_data       , ms_data       , ws_data       ;
wire        es_special_res, rs_special_res, ms_special_res, ws_special_res;
wire        es_rdt_stall  , rs_rdt_stall  , ms_rdt_stall  , ws_rdt_stall  ;

wire 		r1_from_es, r1_from_rs, r1_from_ms, r1_from_ws;
wire 		r2_from_es, r2_from_rs, r2_from_ms, r2_from_ws;

assign {es_rdt_stall, es_tlb_stall, es_special_res, es_valid, es_gr_we, es_dest, es_data} = es_to_ds_bus;
assign {rs_rdt_stall, rs_tlb_stall, rs_special_res, rs_valid, rs_gr_we, rs_dest, rs_data} = rs_to_ds_bus;
assign {ms_rdt_stall, ms_tlb_stall, ms_special_res, ms_valid, ms_gr_we, ms_dest, ms_data} = ms_to_ds_bus;
assign {ws_rdt_stall, ws_tlb_stall, ws_ex, ws_is_ertn, ws_special_res, ws_valid, ws_gr_we, ws_dest, ws_data} = ws_to_ds_bus;
//es forward
assign r1_from_es = rf_raddr1 != 0
				 && es_valid && es_gr_we
				 && rf_raddr1 == es_dest;
assign r2_from_es = rf_raddr2 != 0
				 && es_valid && es_gr_we
				 && rf_raddr2 == es_dest;
//rs forward
assign r1_from_rs = rf_raddr1 != 0
				 && rs_valid && rs_gr_we
				 && rf_raddr1 == rs_dest;
assign r2_from_rs = rf_raddr2 != 0
				 && rs_valid && rs_gr_we
				 && rf_raddr2 == rs_dest;	
//ms forward			 
assign r1_from_ms = rf_raddr1 != 0
				 && ms_valid && ms_gr_we
				 && rf_raddr1 == ms_dest;
assign r2_from_ms = rf_raddr2 != 0
				 && ms_valid && ms_gr_we
				 && rf_raddr2 == ms_dest;
//ws forward
assign r1_from_ws = rf_raddr1 != 0
				 && ws_valid && ws_gr_we
				 && rf_raddr1 == ws_dest;
assign r2_from_ws = rf_raddr2 != 0
				 && ws_valid && ws_gr_we
				 && rf_raddr2 == ws_dest;

// special_res_stall 包括csr_stall/ld_stall
assign special_res_stall = (r1_from_es || r2_from_es) && es_special_res 
						|| (r1_from_rs || r2_from_rs) && rs_special_res
						|| (r1_from_ms || r2_from_ms) && ms_special_res
						|| (r1_from_ws || r2_from_ws) && ws_special_res;
// rdt_stall
assign rdt_stall = (es_rdt_stall & es_valid)
				 | (rs_rdt_stall & rs_valid)
				 | (ms_rdt_stall & ms_valid)
				 | (ws_rdt_stall & ws_valid);
// tlbsrch_stall
assign tlb_stall = es_tlb_stall || rs_tlb_stall || ms_tlb_stall || ws_tlb_stall;

// buffer_stall
/* wire buffer_stall1;
wire buffer_stall2; */
wire buffer_gr_we;
wire [4:0] buffer_dest;
assign buffer_gr_we = ds_buffer[102];
assign buffer_dest = ds_buffer[200:196];
/* assign buffer_stall1 = rf_raddr1 != 0
				 && ds_buffer_valid
				 && buffer_gr_we
				 && rf_raddr1 == buffer_dest;
assign buffer_stall2 = rf_raddr2 != 0
				 && ds_buffer_valid
				 && buffer_gr_we
				 && rf_raddr2 == buffer_dest;
assign buffer_stall = buffer_stall1 || buffer_stall2; */
assign buffer_stall = (rf_raddr1 != 0 || rf_raddr2 != 0)
				 && ds_buffer_valid
				 && buffer_gr_we; //只要buffer写，就stall，后续可着重优化；判断rf_raddr2 == buffer_dest会出现在关键路径上，要想办法解决

assign ds_ready_go    = ws_ex || ws_is_ertn || !(special_res_stall || tlb_stall || br_stall || buffer_stall || rkd_stall || rdt_stall);
assign ds_allowin     = !ds_valid || ds_ready_go && !ds_buffer_valid;
assign ds_to_es_valid = (ds_valid && ds_ready_go ||  ds_buffer_valid) && !es_br_taken; //如果es_br_taken影响时序，那就在exe阶段，当exe出现br_taken，exe下一周期必然无效
always @(posedge clk) begin
	if (reset) begin
        ds_valid        <= 1'b0               ;
		fs_to_ds_bus_r  <= `FS_TO_DS_BUS_WD'b0;
		es_is_lw_r      <= 1'b0               ;
    end
	else begin
		if (ws_ex || ws_is_ertn || es_br_taken) begin
			ds_valid <= 1'b0;
		end
		else if (ds_allowin) begin
			ds_valid <= fs_to_ds_valid && !br_taken;
		end
		else if ((es_allowin || !ds_buffer_valid) && ds_ready_go) begin//;(ds_buffer_valid && es_allowin) && ds_ready_go || !ds_buffer_valid && ds_ready_go
			ds_valid <= 1'b0;
		end
	
		if (fs_to_ds_valid && ds_allowin) begin
			fs_to_ds_bus_r <= (ws_is_ertn || ws_ex) ? `FS_TO_DS_BUS_WD'b0 : fs_to_ds_bus;
		end
	end

	if (reset || ws_ex || ws_is_ertn || es_br_taken) begin
		ds_buffer_valid <= 1'b0;
	end
	else if (ds_buffer_valid) begin
		if (es_allowin) begin
			if (ds_valid && ds_ready_go) begin
				ds_buffer <= ds_buffer_h;
				ds_buffer_valid <= 1'b1;
			end
			else begin
				ds_buffer_valid <= 1'b0;
			end
		end
	end
	else if (/* !ds_buffer_valid &&  */ds_valid && ds_ready_go) begin 
		ds_buffer_valid <= !es_allowin;
		ds_buffer <= ds_buffer_h;
	end
end

assign rd   = ds_inst[ 4: 0];
assign rj   = ds_inst[ 9: 5];
assign rk   = ds_inst[14:10];

assign i12  = ds_inst[21:10];
assign i20  = ds_inst[24: 5];
assign i16  = ds_inst[25:10];
assign i26  = {ds_inst[ 9: 0], ds_inst[25:10]};
assign csr  = ds_inst[23:10];

assign inst_add_w    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h00];
assign inst_sub_w    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h02];
assign inst_slt      = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h04];
assign inst_sltu     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h05];
assign inst_nor      = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h08];
assign inst_and      = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h09];
assign inst_or       = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0a];
assign inst_xor      = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0b];
assign inst_slli_w   = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h01];
assign inst_srli_w   = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h09];
assign inst_srai_w   = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h11];
assign inst_addi_w   = op_31_26_d[6'h00] & op_25_22_d[4'ha];
assign inst_ld_w     = op_31_26_d[6'h0a] & op_25_22_d[4'h2];
assign inst_st_w     = op_31_26_d[6'h0a] & op_25_22_d[4'h6];
assign inst_jirl     = op_31_26_d[6'h13];
assign inst_b        = op_31_26_d[6'h14];
assign inst_bl       = op_31_26_d[6'h15];
assign inst_beq      = op_31_26_d[6'h16];
assign inst_bne      = op_31_26_d[6'h17];
assign inst_lu12i_w  = op_31_26_d[6'h05] & ~ds_inst[25];
assign inst_pcaddu12i= op_31_26_d[6'h07] & ~ds_inst[25];
assign inst_slti     = op_31_26_d[6'h00] & op_25_22_d[4'h8];
assign inst_sltui    = op_31_26_d[6'h00] & op_25_22_d[4'h9];
assign inst_andi     = op_31_26_d[6'h00] & op_25_22_d[4'hd];
assign inst_ori      = op_31_26_d[6'h00] & op_25_22_d[4'he];
assign inst_xori     = op_31_26_d[6'h00] & op_25_22_d[4'hf];
assign inst_sll_w    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0e];
assign inst_sra_w    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h10];
assign inst_srl_w    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0f];
assign inst_div_w    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h00];
assign inst_div_wu   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h02];
assign inst_mul_w    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h18];
assign inst_mulh_w   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h19];
assign inst_mulh_wu  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h1a];
assign inst_mod_w    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h01];
assign inst_mod_wu   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h03];
assign inst_blt      = op_31_26_d[6'h18];
assign inst_bge      = op_31_26_d[6'h19];
assign inst_bltu     = op_31_26_d[6'h1a];
assign inst_bgeu     = op_31_26_d[6'h1b];
assign inst_ld_b     = op_31_26_d[6'h0a] & op_25_22_d[4'h0];
assign inst_ld_h     = op_31_26_d[6'h0a] & op_25_22_d[4'h1];
assign inst_ld_bu    = op_31_26_d[6'h0a] & op_25_22_d[4'h8];
assign inst_ld_hu    = op_31_26_d[6'h0a] & op_25_22_d[4'h9];
assign inst_st_b     = op_31_26_d[6'h0a] & op_25_22_d[4'h4];
assign inst_st_h     = op_31_26_d[6'h0a] & op_25_22_d[4'h5];
assign inst_csrrd    = op_31_26_d[6'h01] & ~ds_inst[25] & ~ds_inst[24] & op_9_5_d[5'h0];
assign inst_csrwr    = op_31_26_d[6'h01] & ~ds_inst[25] & ~ds_inst[24] & op_9_5_d[5'h1];
assign inst_csrxchg  = op_31_26_d[6'h01] & ~ds_inst[25] & ~ds_inst[24] & csrxchg_h;
assign inst_ertn     = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h10] & op_14_10_d[5'h0e] & op_9_5_d[5'h00] & op_4_0_d[5'h00];
assign inst_syscall  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h16];
assign inst_break    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h14];
assign inst_rdtimel_w= op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h0] & op_19_15_d[5'h00] & op_14_10_d[5'h18];
assign inst_rdtimeh_w= op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h0] & op_19_15_d[5'h00] & op_14_10_d[5'h19];
assign inst_tlbsrch  = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h10] & op_14_10_d[5'h0a] & op_9_5_d[5'h00] & op_4_0_d[5'h00];
assign inst_tlbrd    = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h10] & op_14_10_d[5'h0b] & op_9_5_d[5'h00] & op_4_0_d[5'h00];
assign inst_tlbwr    = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h10] & op_14_10_d[5'h0c] & op_9_5_d[5'h00] & op_4_0_d[5'h00];
assign inst_tlbfill  = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h10] & op_14_10_d[5'h0d] & op_9_5_d[5'h00] & op_4_0_d[5'h00];
assign inst_invtlb   = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h13];
assign inst_cacop    = op_31_26_d[6'h01] & op_25_22_d[4'h8];

assign alu_op[ 0] = inst_add_w | inst_addi_w | inst_ld_w      | inst_st_w
                  | inst_jirl  | inst_bl     | inst_pcaddu12i | inst_ld_b
				  | inst_ld_h  | inst_ld_bu  | inst_ld_hu     | inst_st_b 
				  | inst_st_h  | inst_csrwr  | inst_csrxchg   | inst_cacop;
assign alu_op[ 1] = inst_sub_w;
assign alu_op[ 2] = inst_slt    | inst_slti;
assign alu_op[ 3] = inst_sltu   | inst_sltui;
assign alu_op[ 4] = inst_and    | inst_andi;
assign alu_op[ 5] = inst_nor;
assign alu_op[ 6] = inst_or     | inst_ori;
assign alu_op[ 7] = inst_xor    | inst_xori;
assign alu_op[ 8] = inst_slli_w | inst_sll_w;
assign alu_op[ 9] = inst_srli_w | inst_srl_w;
assign alu_op[10] = inst_srai_w | inst_sra_w;
assign alu_op[11] = inst_lu12i_w;
assign alu_op[12] = inst_div_w;
assign alu_op[13] = inst_div_wu;
assign alu_op[14] = inst_mul_w;
assign alu_op[15] = inst_mulh_w;
assign alu_op[16] = inst_mulh_wu;
assign alu_op[17] = inst_mod_w;
assign alu_op[18] = inst_mod_wu;

assign need_ui5   =  inst_slli_w  | inst_srli_w | inst_srai_w;
assign need_si12  =  inst_addi_w  | inst_ld_w   | inst_st_w  | inst_slti  | inst_sltui 
					 | inst_ld_b  | inst_ld_h   | inst_ld_bu | inst_ld_hu | inst_st_b | inst_st_h | inst_cacop;
assign need_si20  =  inst_lu12i_w | inst_pcaddu12i;
assign need_si26  =  inst_b       | inst_bl;
assign need_ui12  = inst_andi     | inst_ori    | inst_xori;
assign src2_is_4  =  inst_jirl    | inst_bl;

assign imm = {32{src2_is_4}} & 32'h4
		   | {32{need_si20}} & {i20, 12'b0} 
		   | {32{need_si12}} & {{20{i12[11]}}, i12[11:0]}
		   | {32{need_ui12}} & {20'b0, i12}
		   | {32{need_ui5}}  & {27'b0, rk};

assign br_offs = need_si26 ? {{ 4{i26[25]}}, i26[25:0], 2'b0} :
                             {{14{i16[15]}}, i16[15:0], 2'b0} ;

assign jirl_offs = {{14{i16[15]}}, i16[15:0], 2'b0};

assign src_reg_is_rd = inst_beq | inst_bne | inst_st_w | inst_blt | inst_bge | inst_bltu | inst_bgeu | inst_st_b | inst_st_h | inst_csrwr | inst_csrxchg;

assign src1_is_pc    = inst_jirl | inst_bl | inst_pcaddu12i;

assign src2_is_imm   = inst_slli_w   | inst_srli_w   | inst_srai_w   | inst_addi_w   | inst_ld_w     | inst_st_w     |
                       inst_lu12i_w  | inst_jirl     | inst_bl       | inst_slti     | inst_pcaddu12i| inst_sltui    |
					   inst_andi     | inst_ori      | inst_xori     | inst_ld_b     | inst_ld_h     | inst_ld_bu    |
					   inst_ld_hu    | inst_st_b     | inst_st_h     | inst_cacop    ;

assign dst_is_r1     = inst_bl;
assign gr_we         = ~inst_st_w & ~inst_beq  & ~inst_bne     & ~inst_b     & ~inst_blt   & ~inst_bge     & ~inst_bltu   & ~inst_bgeu 
					 & ~inst_st_b & ~inst_st_h & ~inst_tlbsrch & ~inst_tlbrd & ~inst_tlbwr & ~inst_tlbfill & ~inst_invtlb & ~inst_cacop;
assign mem_we        = inst_st_w | inst_st_b | inst_st_h;
assign dest          =/*  (inst_rdtimeh_w|inst_rdtimel_w) ? rj :  */dst_is_r1 ? 5'd1 : rd;

assign rf_raddr1 = rj;
assign rf_raddr2 = src_reg_is_rd ? rd :rk;

regfile u_regfile(
    .clk     (clk       ),
    .raddr1  (rf_raddr1 ),
    .rdata1  (rf_rdata1 ),
    .raddr2  (rf_raddr2 ),
    .rdata2  (rf_rdata2 ),
	.rdt_req (rdt_req   ),
	.rdt_addr(rdt_addr  ),
	.rdt_data(rdt_data  ),
    .we      (rf_we     ),
    .waddr   (rf_waddr  ),
    .wdata   (rf_wdata  )
	`ifdef DIFFTEST_EN
	,
	.regs    (rf_to_diff)
	`endif
    );

assign rj_value = r1_from_es ? es_data :
				  r1_from_rs ? rs_data :
				  r1_from_ms ? ms_data : 
				  r1_from_ws ? ws_data : rf_rdata1;
				  
assign rkd_value= r2_from_es ? es_data : 
				  r2_from_rs ? rs_data :
				  r2_from_ms ? ms_data : rf_rdata2;
assign rkd_stall = r2_from_ws;

assign rj_eq_rd = (rf_rdata1 == rf_rdata2);
assign rj_lt_rd = $signed(rf_rdata1) < $signed(rf_rdata2);
assign rj_lt_rd_u = rf_rdata1 < rf_rdata2;

assign br_stall = (inst_beq || inst_bne || inst_blt || inst_bge || inst_bltu || inst_bgeu || inst_jirl)
			   && ((r1_from_es || r1_from_ms || r1_from_rs || r1_from_ws)
			    || (r2_from_es || r2_from_ms || r2_from_rs || r2_from_ws));
assign br_taken = (   inst_beq  &&  rj_eq_rd
                   || inst_bne  && !rj_eq_rd
				   || inst_blt  &&  rj_lt_rd
				   || inst_bge  && !rj_lt_rd
				   || inst_bltu &&  rj_lt_rd_u
				   || inst_bgeu && !rj_lt_rd_u
                   || inst_jirl
                   || inst_bl
                   || inst_b
                  ) && ds_valid && !special_res_stall && ds_ready_go;
assign br_target = (inst_beq || inst_bne || inst_bl || inst_b || inst_blt || inst_bge || inst_bltu || inst_bgeu) ? (ds_pc + br_offs) :
                                                   /*inst_jirl*/ (rf_rdata1 + jirl_offs);

assign ds_refetch = (inst_tlbwr || inst_tlbfill || ((inst_csrwr || inst_csrxchg) && ((csr == 5'h0) || (csr == 5'h18)))) && ds_valid;

endmodule