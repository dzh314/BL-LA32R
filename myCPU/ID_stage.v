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
    //to ps
    output [`BR_BUS_WD       -1:0] br_bus        ,
	//to fs
	output                         ds_to_fs_bus  ,
    //to rf: for write back
    input  [`WS_TO_RF_BUS_WD -1:0] ws_to_rf_bus  ,
	//input
	input  [`ES_TO_DS_BUS_WD -1:0] es_to_ds_bus  ,
	input  [`MS_TO_DS_BUS_WD -1:0] ms_to_ds_bus  ,
	input  [`WS_TO_DS_BUS_WD -1:0] ws_to_ds_bus  ,
	//refetch
	output                         ds_refetch    ,
	//cacop
	output                         ds_is_icacop       //没有与br_bus合并，因为担心加了分支预测会影响br_bus 
);

reg         ds_valid    ;
wire        ds_ready_go ;

reg         bran_delay_r;
wire		ds_ex		;
wire [5:0]  ds_ecode    ;
wire        fs_ex       ;
wire [5:0]  fs_ecode    ;
wire        s0_ex   ;
wire        s0_refill_ex;
wire        ws_ex       ;
wire		ws_is_ertn  ;
wire		is_instr    ;
wire es_tlb_stall, ms_tlb_stall, ws_tlb_stall;

wire [31                 :0] fs_pc;
reg  [`FS_TO_DS_BUS_WD -1:0] fs_to_ds_bus_r;
reg			                 es_is_lw_r;
assign fs_pc = fs_to_ds_bus[31:0];
wire ds_is_icacop_h;
assign ds_is_icacop = ds_is_icacop_h && ds_valid;

wire [31:0] ds_inst;
wire [31:0] ds_pc  ;
assign {ds_is_icacop_h,
		s0_ex       ,
		s0_refill_ex,
		fs_ecode    ,
		fs_ex       ,
		ds_inst     ,
        ds_pc  
		} = fs_to_ds_bus_r;

wire        rf_we   ;
wire [ 4:0] rf_waddr;
wire [31:0] rf_wdata;
assign {rf_we   ,  //37:37
        rf_waddr,  //36:32
        rf_wdata   //31:0
       } = ws_to_rf_bus;

wire        br_taken;
wire [31:0] br_target;

wire [18:0] alu_op;
wire        src1_is_pc;
wire        src2_is_imm;/* 
wire        res_from_mem; */
wire        dst_is_r1;
wire        src_reg_is_rd;
wire        gr_we;
wire        mem_we;
wire [ 4:0] dest;
wire [31:0] imm;
wire [31:0] rj_value;
wire [31:0] rkd_value;
wire [31:0] br_offs;
wire [31:0] jirl_offs;

////////////////******************
wire [ 5:0] op_31_26;
wire [ 3:0] op_25_22;
wire [ 1:0] op_21_20;
wire [ 4:0] op_19_15;
wire [ 4:0] rd;
wire [ 4:0] rj;
wire [ 4:0] rk;
wire [11:0] i12;
wire [19:0] i20;
wire [15:0] i16;
wire [25:0] i26;
wire [13:0] csr;

wire [63:0] op_31_26_d;
wire [15:0] op_25_22_d;
wire [ 3:0] op_21_20_d;
wire [31:0] op_19_15_d;

wire        inst_add_w;
wire        inst_sub_w;
wire        inst_slt;
wire        inst_sltu;
wire        inst_nor;
wire        inst_and;
wire        inst_or;
wire        inst_xor;
wire        inst_slli_w;
wire        inst_srli_w;
wire        inst_srai_w;
wire        inst_addi_w;
wire        inst_ld_w;
wire        inst_st_w;
wire        inst_jirl;
wire        inst_b;
wire        inst_bl;
wire        inst_beq;
wire        inst_bne;
wire        inst_lu12i_w;
wire		inst_pcaddu12i;
wire		inst_slti;
wire		inst_sltui;
wire		inst_andi;
wire		inst_ori;
wire		inst_xori;
wire		inst_sll_w;
wire		inst_sra_w;
wire		inst_srl_w;
wire		inst_div_w;
wire		inst_div_wu;
wire		inst_mul_w;
wire		inst_mulh_w;
wire		inst_mulh_wu;
wire		inst_mod_w;
wire		inst_mod_wu;
wire		inst_blt;
wire		inst_bge;
wire		inst_bltu;
wire		inst_bgeu;
wire		inst_ld_b;
wire        inst_ld_h;
wire		inst_ld_bu;
wire		inst_ld_hu;
wire		inst_st_b;
wire		inst_st_h;
wire		inst_csrrd;
wire		inst_csrwr;
wire		inst_csrxchg;
wire		inst_ertn;
wire		inst_syscall;
wire		inst_break;
wire		inst_rdtimel_w;
wire		inst_rdtimeh_w;
wire		inst_tlbsrch;
wire		inst_tlbrd;
wire		inst_tlbwr;
wire		inst_tlbfill;
wire		inst_invtlb;
wire        inst_cacop;

wire invtlb_op_valid;
assign invtlb_op_valid = !(rd[4] || rd[3]) && !(rd[2] && rd[1] && rd[0]);

assign is_instr = inst_add_w|inst_sub_w|inst_slt|inst_sltu|inst_nor|inst_and|inst_or
				 |inst_xor|inst_slli_w|inst_srli_w|inst_srai_w|inst_addi_w|inst_ld_w
				 |inst_st_w|inst_jirl|inst_b|inst_bl|inst_beq|inst_bne|inst_lu12i_w
				 |inst_pcaddu12i|inst_slti|inst_sltui|inst_andi|inst_ori|inst_xori
				 |inst_sll_w|inst_sra_w|inst_srl_w|inst_div_w|inst_div_wu|inst_mul_w
				 |inst_mulh_w|inst_mulh_wu|inst_mod_w|inst_mod_wu|inst_blt|inst_bge
				 |inst_bltu|inst_bgeu|inst_ld_b|inst_ld_h|inst_ld_bu|inst_ld_hu
				 |inst_st_b|inst_st_h|inst_csrrd|inst_csrwr|inst_csrxchg|inst_ertn
				 |inst_syscall|inst_break|inst_rdtimel_w|inst_rdtimeh_w|inst_tlbsrch
				 |inst_tlbrd|inst_tlbwr|inst_tlbfill|(inst_invtlb && invtlb_op_valid)
				 |inst_cacop;

wire        need_ui5;
wire        need_si12;
wire        need_si16;
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

assign ds_ex = ( fs_ex | inst_syscall | inst_break | (!is_instr && !(ws_ex || ws_is_ertn)) ) && ds_valid;
assign ds_ecode = fs_ex ? fs_ecode :
				  (!is_instr) ? 6'h0d : 
				  inst_syscall ? 6'h0b : 
				  inst_break ? 6'h0c : 
				  6'h00;

assign br_bus       = {br_taken,br_target};

assign ds_to_es_bus = {inst_cacop    , //230
					   ds_is_icacop  , //229
					   s0_ex         , //228
					   s0_refill_ex  , //227
					   ds_refetch    , //226
					   rd            , //225:221
					   inst_tlbsrch  , //220
					   inst_tlbrd    , //219
					   inst_tlbwr    , //218
					   inst_tlbfill  , //217
					   inst_invtlb   , //216
					   inst_rdtimeh_w, //215
					   inst_rdtimel_w, //214
					   ds_ecode      , //213:208
					   ds_ex         , //207
					   inst_ertn     , //206
					   inst_csrxchg  , //205
					   inst_csrwr    , //204
					   inst_csrrd    , //203
					   csr           , //202:189
					   inst_st_h     , //188
					   inst_st_b     , //187
					   inst_ld_hu    , //186:186
					   inst_ld_bu    , //186:186
					   inst_ld_h     , //185:185
					   inst_ld_b     , //184:184
					   inst_ld_w     , //183:183
					   imm           , //182:151
					   src2_is_imm   , //150:150
					   src1_is_pc    , //149:149
					   alu_op        , //148:131
					   inst_ld_w     , //130:130
					   gr_we         , //129:129
					   mem_we        , //128:128
					   dest          , //127:96
					   rj_value      , //95 :64
					   rkd_value     , //63 :32
					   ds_pc           //31 :0
};

// forward unit
wire		es_is_ld;
wire 	    es_valid, ms_valid, ws_valid;
wire	    es_gr_we, ms_gr_we,	ws_gr_we;
wire [4 :0] es_dest , ms_dest , ws_dest ;
wire [31:0] es_data , ms_data , ws_data ;
wire 		r1_from_es, r1_from_ms, r1_from_ws;
wire 		r2_from_es, r2_from_ms, r2_from_ws;
wire		es_res_from_csr, ms_res_from_csr;
assign {es_tlb_stall, es_res_from_rdtime, es_res_from_csr, es_is_ld, es_valid, es_gr_we, es_dest, es_data} = es_to_ds_bus;
assign {ms_tlb_stall, ms_res_from_rdtime, ms_res_from_csr, ms_valid, ms_gr_we, ms_dest, ms_data} = ms_to_ds_bus;
assign {ws_tlb_stall, ws_ex, ws_is_ertn, ws_valid, ws_gr_we, ws_dest, ws_data} = ws_to_ds_bus;

// csr_stall
wire csr_stall;
assign csr_stall = (r1_from_es || r2_from_es) && es_res_from_csr || (r1_from_ms || r2_from_ms) && ms_res_from_csr;
// rdtime_stall
wire rdtime_stall;
assign rdtime_stall = (r1_from_es || r2_from_es) && es_res_from_rdtime || (r1_from_ms || r2_from_ms) && ms_res_from_rdtime;
// ld_w_stall
wire ld_w_stall;
assign ld_w_stall = es_is_ld && (r1_from_es || r2_from_es);
// tlbsrch_stall
wire tlb_stall;
assign tlb_stall = es_tlb_stall || ms_tlb_stall || ws_tlb_stall;

assign ds_ready_go = ws_ex || ws_is_ertn || !(ld_w_stall || csr_stall || rdtime_stall || tlb_stall);
assign ds_allowin     = !ds_valid || ds_ready_go && es_allowin;
assign ds_to_es_valid = ds_valid && ds_ready_go;
always @(posedge clk) begin
	if (reset) begin
        ds_valid <= 1'b0;
		fs_to_ds_bus_r <= `FS_TO_DS_BUS_WD'b0;
		es_is_lw_r     <= 1'b0;
		bran_delay_r   <= 1'b0;
    end
	else begin
		if (ws_ex || ws_is_ertn) begin
			ds_valid <= 1'b0;
		end
		else if (ds_allowin) begin
			ds_valid <= fs_to_ds_valid;
		end
	
		if (fs_to_ds_valid && ds_allowin) begin
			fs_to_ds_bus_r <= (ws_is_ertn || ws_ex) ? `FS_TO_DS_BUS_WD'b0 : fs_to_ds_bus;
		end
	end
end

assign op_31_26  = ds_inst[31:26];
assign op_25_22  = ds_inst[25:22];
assign op_21_20  = ds_inst[21:20];
assign op_19_15  = ds_inst[19:15];

assign rd   = ds_inst[ 4: 0];
assign rj   = ds_inst[ 9: 5];
assign rk   = ds_inst[14:10];

assign i12  = ds_inst[21:10];
assign i20  = ds_inst[24: 5];
assign i16  = ds_inst[25:10];
assign i26  = {ds_inst[ 9: 0], ds_inst[25:10]};
assign csr  = ds_inst[23:10];

decoder_6_64 u_dec0(.in(op_31_26 ), .out(op_31_26_d ));
decoder_4_16 u_dec1(.in(op_25_22 ), .out(op_25_22_d ));
decoder_2_4  u_dec2(.in(op_21_20 ), .out(op_21_20_d ));
decoder_5_32 u_dec3(.in(op_19_15 ), .out(op_19_15_d ));

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
assign inst_csrrd    = op_31_26_d[6'h01] & ~ds_inst[25] & ~ds_inst[24] & (rj == 5'b0);
assign inst_csrwr    = op_31_26_d[6'h01] & ~ds_inst[25] & ~ds_inst[24] & (rj == 5'b1);
assign inst_csrxchg  = op_31_26_d[6'h01] & ~ds_inst[25] & ~ds_inst[24] & (rj[4:1] != 4'b0);
assign inst_ertn     = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h10] & (rk == 5'h0e) & (rj == 5'h00) & (rd == 5'h00);
assign inst_syscall  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h16];
assign inst_break    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h14];
assign inst_rdtimel_w= op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h0] & op_19_15_d[5'h00] & (rk == 5'h18);
assign inst_rdtimeh_w= op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h0] & op_19_15_d[5'h00] & (rk == 5'h19);
assign inst_tlbsrch  = (ds_inst == 32'h06482800); // 32'b0000 0110 0100 1000 0010 1000 0000 0000
assign inst_tlbrd    = (ds_inst == 32'h06482c00); // 32'b0000 0110 0100 1000 0010 1100 0000 0000
assign inst_tlbwr    = (ds_inst == 32'h06483000); // 32'b0000 0110 0100 1000 0011 0000 0000 0000
assign inst_tlbfill  = (ds_inst == 32'h06483400); // 32'b0000 0110 0100 1000 0011 0100 0000 0000
assign inst_invtlb   = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h13];
assign inst_cacop    = op_31_26_d[6'h01] & op_25_22_d[4'h8];

assign alu_op[ 0] = inst_add_w | inst_addi_w | inst_ld_w | inst_st_w
                    | inst_jirl | inst_bl | inst_pcaddu12i | inst_ld_b
					| inst_ld_h | inst_ld_bu | inst_ld_hu | inst_st_b | inst_st_h | inst_csrwr | inst_csrxchg | inst_cacop;
assign alu_op[ 1] = inst_sub_w;
assign alu_op[ 2] = inst_slt | inst_slti;
assign alu_op[ 3] = inst_sltu | inst_sltui;
assign alu_op[ 4] = inst_and | inst_andi;
assign alu_op[ 5] = inst_nor;
assign alu_op[ 6] = inst_or | inst_ori;
assign alu_op[ 7] = inst_xor | inst_xori;
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

assign need_ui5   =  inst_slli_w | inst_srli_w | inst_srai_w;
assign need_si12  =  inst_addi_w | inst_ld_w | inst_st_w | inst_slti | inst_sltui 
					 | inst_ld_b | inst_ld_h | inst_ld_bu | inst_ld_hu | inst_st_b | inst_st_h | inst_cacop;
assign need_si16  =  inst_jirl | inst_beq | inst_bne | inst_blt | inst_bge | inst_bltu | inst_bgeu;
assign need_si20  =  inst_lu12i_w | inst_pcaddu12i;
assign need_si26  =  inst_b | inst_bl;
assign need_ui12  = inst_andi | inst_ori | inst_xori;
assign src2_is_4  =  inst_jirl | inst_bl;

assign imm = {32{src2_is_4}} & 32'h4
		   | {32{need_si20}} & {i20[19:0], 12'b0} 
		   | {32{need_si12}} & {{20{i12[11]}}, i12[11:0]}
		   | {32{need_ui12}} & {20'b0, i12}
		   | {32{need_ui5}}  & {27'b0, rk};
/* assign imm = src2_is_4 ? 32'h4                      :
             need_si20 ? {i20[19:0], 12'b0}         :
             need_si12 ? {{20{i12[11]}}, i12[11:0]} :
			 need_ui12 ? {20'b0, i12}               :
		   /* need_ui5 {27'b0, rk}                ; */
  

assign br_offs = need_si26 ? {{ 4{i26[25]}}, i26[25:0], 2'b0} :
                             {{14{i16[15]}}, i16[15:0], 2'b0} ;

assign jirl_offs = {{14{i16[15]}}, i16[15:0], 2'b0};

assign src_reg_is_rd = inst_beq | inst_bne | inst_st_w | inst_blt | inst_bge | inst_bltu | inst_bgeu | inst_st_b | inst_st_h | inst_csrwr | inst_csrxchg;

assign src1_is_pc    = inst_jirl | inst_bl | inst_pcaddu12i;

assign src2_is_imm   = inst_slli_w   |
                       inst_srli_w   |
                       inst_srai_w   |
                       inst_addi_w   |
                       inst_ld_w     |
                       inst_st_w     |
                       inst_lu12i_w  |
                       inst_jirl     |
                       inst_bl       |
					   inst_slti     |
					   inst_pcaddu12i|
					   inst_sltui    |
					   inst_andi     |
					   inst_ori      |
					   inst_xori     |
					   inst_ld_b     |
					   inst_ld_h     |
					   inst_ld_bu    |
					   inst_ld_hu    |
					   inst_st_b     |
					   inst_st_h     |
					   inst_cacop    ;

/* assign res_from_mem  = inst_ld_w | inst_ld_b | inst_ld_h | inst_ld_bu | inst_ld_hu; */
assign dst_is_r1     = inst_bl;
assign gr_we         = ~inst_st_w & ~inst_beq & ~inst_bne & ~inst_b & ~inst_blt & ~inst_bge & ~inst_bltu & ~inst_bgeu 
					 & ~inst_st_b & ~inst_st_h & ~inst_tlbsrch & ~inst_tlbrd & ~inst_tlbwr & ~inst_tlbfill & ~inst_invtlb & ~inst_cacop;
assign mem_we        = inst_st_w | inst_st_b | inst_st_h;
assign dest          = (inst_rdtimeh_w|inst_rdtimel_w) ? rj : dst_is_r1 ? 5'd1 : rd;

assign rf_raddr1 = rj;
assign rf_raddr2 = src_reg_is_rd ? rd :rk;

regfile u_regfile(
    .clk    (clk      ),
    .raddr1 (rf_raddr1),
    .rdata1 (rf_rdata1),
    .raddr2 (rf_raddr2),
    .rdata2 (rf_rdata2),
    .we     (rf_we    ),
    .waddr  (rf_waddr ),
    .wdata  (rf_wdata )
    );

// forward unit
assign r1_from_es = rf_raddr1 != 0
				 && es_valid
				 && es_gr_we
				 && rf_raddr1 == es_dest;
assign r2_from_es = rf_raddr2 != 0
				 && es_valid
				 && es_gr_we
				 && rf_raddr2 == es_dest;
assign r1_from_ms = rf_raddr1 != 0
				 && ms_valid
				 && ms_gr_we
				 && rf_raddr1 == ms_dest;
assign r2_from_ms = rf_raddr2 != 0
				 && ms_valid
				 && ms_gr_we
				 && rf_raddr2 == ms_dest;
assign r1_from_ws = rf_raddr1 != 0
				 && ws_valid
				 && ws_gr_we
				 && rf_raddr1 == ws_dest;
assign r2_from_ws = rf_raddr2 != 0
				 && ws_valid
				 && ws_gr_we
				 && rf_raddr2 == ws_dest;

assign rj_value = r1_from_es ? es_data : 
				  r1_from_ms ? ms_data : 
				  r1_from_ws ? ws_data : rf_rdata1;
				  
assign rkd_value= r2_from_es ? es_data : 
				  r2_from_ms ? ms_data : 
				  r2_from_ws ? ws_data : rf_rdata2;

assign rj_eq_rd = (rj_value == rkd_value);
assign rj_lt_rd = $signed(rj_value) < $signed(rkd_value);
assign rj_lt_rd_u = rj_value < rkd_value;
assign br_taken = (   inst_beq  &&  rj_eq_rd
                   || inst_bne  && !rj_eq_rd
				   || inst_blt  &&  rj_lt_rd
				   || inst_bge  && !rj_lt_rd
				   || inst_bltu &&  rj_lt_rd_u
				   || inst_bgeu && !rj_lt_rd_u
                   || inst_jirl
                   || inst_bl
                   || inst_b
                  ) && ds_valid && !(ld_w_stall || csr_stall || rdtime_stall);
assign br_target = (inst_beq || inst_bne || inst_bl || inst_b || inst_blt || inst_bge || inst_bltu || inst_bgeu) ? (ds_pc + br_offs) :
                                                   /*inst_jirl*/ (rj_value + jirl_offs);
assign ds_to_fs_bus = br_taken;

assign ds_refetch = (inst_tlbwr || inst_tlbfill || ((inst_csrwr || inst_csrxchg) && ((csr == 5'h0) || (csr == 5'h18)))) && ds_valid;

endmodule
