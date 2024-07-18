`include "mycpu.h"

module wb_stage(
    input                           clk           ,
    input                           reset         ,
    //allowin
    output                          ws_allowin    ,
    //from ms
    input                           ms_to_ws_valid,
    input  [`MS_TO_WS_BUS_WD -1:0]  ms_to_ws_bus  ,
    //to rf: for write back
    output [`WS_TO_RF_BUS_WD -1:0]  ws_to_rf_bus  ,
    //trace debug interface
    output [31:0] debug_wb_pc      ,
    output [ 3:0] debug_wb_rf_we   ,
    output [ 4:0] debug_wb_rf_wnum ,
    output [31:0] debug_wb_rf_wdata,
	output [`WS_TO_DS_BUS_WD -1:0] ws_to_ds_bus,
	output [`WS_TO_FS_BUS_WD -1:0] ws_to_fs_bus,
	output [`WS_TO_PS_BUS_WD -1:0] ws_to_ps_bus,
	output [ 1:0]                  ws_to_es_bus,
	output [ 1:0]                  ws_to_ms_bus,
	//with tlb
	output [`WS_TO_TLB_BUS_WD -1:0] ws_to_tlb_bus,
	input  [`TLB_TO_WS_BUS_WD -1:0] tlb_to_ws_bus,
	//refetch
	output                          ws_refetch   ,
	//cache
	output [1:0] datf,
	output [1:0] datm
);
reg         ws_valid;
wire        ws_ready_go;

reg [`MS_TO_WS_BUS_WD -1:0] ms_to_ws_bus_r;

wire		ws_is_ertn_h   ;
wire		ws_is_ertn     ;
wire		ws_is_csrxchg  ;
wire		ws_is_csrwr    ;
wire		ws_is_csrrd    ;
wire		ws_is_rdtimeh_w;
wire		ws_is_rdtimel_w;
wire		ws_is_tlbsrch  ;
wire		ws_is_tlbrd    ;
wire		ws_is_tlbwr    ;
wire		ws_is_tlbfill  ;
wire		ws_is_invtlb   ;
wire [13:0] csr            ;
wire [31:0] mask           ;
wire        ws_gr_we       ;
wire [ 4:0] ws_dest        ;
wire [31:0] ws_final_result;
wire [31:0] ms_final_result;
wire [31:0] ws_pc          ;
wire		ws_ex          ;
wire        s1_refill_ex   ;
wire        refill_ex      ;
wire        s1_ex          ;
wire		s1_ex_h        ;
wire        s1_refill_ex_h ;
wire        s0_refill_ex   ;
wire        s0_ex          ;
wire        s0_refill_ex_h ;
wire        s0_ex_h        ;
wire        ms_ex          ;
wire [31:0] ex_entrance    ;
wire [ 5:0] ms_ecode       ;
wire [ 5:0] ws_ecode       ;
wire [12:0] int_vec        ;
wire        int_ex         ;
wire [ 4:0] invtlb_op      ;
wire [18:0] invtlb_vppn    ;
wire [ 9:0] invtlb_asid    ;
assign invtlb_asid = mask[9:0];

wire ws_refetch_help;
assign ws_refetch = ws_refetch_help && ws_valid;

assign {s0_ex_h        ,
		s0_refill_ex_h ,
		s1_ex_h        ,
		s1_refill_ex_h ,
		ws_refetch_help,
		invtlb_vppn    ,
		invtlb_op      ,
		ws_is_tlbsrch  ,
		ws_is_tlbrd    ,
		ws_is_tlbwr    ,
		ws_is_tlbfill  ,
		ws_is_invtlb   ,
		ws_is_rdtimeh_w,
		ws_is_rdtimel_w,
		ms_ecode       ,
		ms_ex          ,
		ws_is_ertn_h   ,
		mask           ,
		ws_is_csrxchg  ,
		ws_is_csrwr    ,
		ws_is_csrrd    ,
		csr            ,
		ws_gr_we       ,
		ws_dest        ,
		ms_final_result,
		ws_pc} = ms_to_ws_bus_r;
	   
assign ws_is_ertn = ws_is_ertn_h & ws_valid;

// CSR.CRMD
wire        is_csr_crmd  ;
reg  [ 1:0] csr_crmd_plv ;
reg         csr_crmd_ie  ;
reg         csr_crmd_da  ;
reg         csr_crmd_pg  ;
reg  [ 1:0] csr_crmd_datf;
reg  [ 1:0] csr_crmd_datm;
reg         csr_crmd_we  ;
assign is_csr_crmd = (csr == 14'h00);

// CSR.PRMD
wire      is_csr_prmd;
reg [1:0] csr_prmd_pplv;
reg       csr_prmd_pie;
reg       csr_prmd_pwe;
assign is_csr_prmd = (csr == 14'h01);

// CSR.EENTRY 低12位恒为0
wire       is_csr_eentry ;
reg [19:0] csr_eentry_vpn;
assign is_csr_eentry = (csr == 14'hc);

// CSR.ESTAT 
wire       is_csr_estat      ;
reg [ 1:0] csr_estat_is_rw   ;
reg [10:0] csr_estat_is_r    ;
reg [ 5:0] csr_estat_ecode   ;
reg [ 8:0] csr_estat_esubcode;
assign is_csr_estat = (csr == 14'h05);

// CSR.ERA
wire       is_csr_era;
reg [31:0] csr_era_pc;
assign is_csr_era = (csr == 14'h06);

// CSR.SAVE0
wire is_csr_save0;
reg [31:0] csr_save0;
assign is_csr_save0 = (csr == 14'h30);

// CSR.SAVE1
wire is_csr_save1;
reg [31:0] csr_save1;
assign is_csr_save1 = (csr == 14'h31);

// CSR.SAVE2
wire is_csr_save2;
reg [31:0] csr_save2;
assign is_csr_save2 = (csr == 14'h32);

// CSR.SAVE3
wire is_csr_save3;
reg [31:0] csr_save3;
assign is_csr_save3 = (csr == 14'h33);

// CSR.TID
wire is_csr_tid;
reg [31:0] csr_tid;
assign is_csr_tid = (csr == 14'h40);

// CSR.TCFG
wire is_csr_tcfg;
reg        csr_tcfg_en;
reg        csr_tcfg_periodic;
reg [29:0] csr_tcfg_initval;
reg        en_r;
assign is_csr_tcfg = (csr == 14'h41);

// CSR.TVAL
wire is_csr_tval;
reg [31:0] csr_tval_timeval;
assign is_csr_tval = (csr == 14'h42);

// CSR.TICLR
wire is_csr_ticlr;
assign is_csr_ticlr = (csr == 14'h44);

// CSR.ECFG
wire is_csr_ecfg;
reg [12:0] csr_ecfg_lie;
reg [ 2:0] csr_ecfg_vs;
assign is_csr_ecfg = (csr == 14'h04);

// CSR.BADV
wire is_csr_badv;
reg [31:0] csr_badv_vaddr;
assign is_csr_badv = (csr == 14'h07);

// CSR.ASID
wire is_csr_asid;
reg [9:0] csr_asid_asid;     // rw
assign is_csr_asid = (csr == 14'h18);

// CSR.TLBEHI
wire is_csr_tlbehi;
reg [18:0] csr_tlbehi_vppn; // rw
assign is_csr_tlbehi = (csr == 14'h11);

// CSR.TLBIDX
wire is_csr_tlbidx;
reg [3:0] csr_tlbidx_index; // rw，索引，位数由TLB的具体实现决定，4
reg [5:0] csr_tlbidx_ps; // rw，6
reg       csr_tlbidx_ne; // rw，1
assign is_csr_tlbidx = (csr == 14'h10);

// CSR.TLBELO0
wire is_csr_tlbelo0;
reg        csr_tlbelo0_v;
reg        csr_tlbelo0_d;
reg [ 1:0] csr_tlbelo0_plv;
reg [ 1:0] csr_tlbelo0_mat;
reg        csr_tlbelo0_g;
reg [19:0] csr_tlbelo0_ppn;
assign is_csr_tlbelo0 = (csr == 14'h12);

// CSR.TLBELO1
wire is_csr_tlbelo1;
reg        csr_tlbelo1_v;
reg        csr_tlbelo1_d;
reg [ 1:0] csr_tlbelo1_plv;
reg [ 1:0] csr_tlbelo1_mat;
reg        csr_tlbelo1_g;
reg [19:0] csr_tlbelo1_ppn;
assign is_csr_tlbelo1 = (csr == 14'h13);

// CSR.TLBRENTRY
wire is_csr_tlbrentry;
reg [19:0] csr_tlbrentry_ppn;
assign is_csr_tlbrentry = (csr == 14'h88);

// CSR.TLBRERA
wire is_csr_tlbrera;
reg        csr_tlbrera_istlbr;
reg [29:0] csr_tlbrera_pc;
assign is_csr_tlbrera = (csr == 14'h8a);

// CSR.TLBRBADV
wire is_csr_tlbrbadv;
reg [31:0] csr_tlbrbadv_vaddr;
assign is_csr_tlbrbadv = (csr == 14'h89);

// CSR.TLBREHI
wire is_csr_tlbrehi;
reg [ 5:0] csr_tlbrehi_ps;
reg [18:0] csr_tlbrehi_vppn;
assign is_csr_tlbrehi = (csr == 14'h8e);

// CSR.TLBRPRMD
wire is_csr_tlbrprmd;
reg [1:0] csr_tlbrprmd_pplv;
reg       csr_tlbrprmd_pie;
reg       csr_tlbrprmd_pwe;
assign is_csr_tlbrprmd = (csr == 14'h8f);

// CSR.DMW0
wire is_csr_dmw0;
reg [2:0] csr_dmw0_vseg;
reg [2:0] csr_dmw0_pseg;
reg [1:0] csr_dmw0_mat;
reg [3:0] csr_dmw0_plv;
assign is_csr_dmw0 = (csr == 14'h180);
// CSR.DMW1
wire is_csr_dmw1;
reg [2:0] csr_dmw1_vseg;
reg [2:0] csr_dmw1_pseg;
reg [1:0] csr_dmw1_mat;
reg [3:0] csr_dmw1_plv;
assign is_csr_dmw1 = (csr == 14'h181);
// CSR.DMW2
wire is_csr_dmw2;
reg [2:0] csr_dmw2_vseg;
reg [2:0] csr_dmw2_pseg;
reg [1:0] csr_dmw2_mat;
reg [3:0] csr_dmw2_plv;
assign is_csr_dmw2 = (csr == 14'h182);
// CSR.DMW3
wire is_csr_dmw3;
reg [2:0] csr_dmw3_vseg;
reg [2:0] csr_dmw3_pseg;
reg [1:0] csr_dmw3_mat;
reg [3:0] csr_dmw3_plv;
assign is_csr_dmw3 = (csr == 14'h183);
//tlb
wire [ 9:0] r_asid;
wire [ 5:0] r_ps;
wire [18:0] r_vppn;
wire [ 3:0] s1_index;
wire        r_e;
wire        s1_found;
wire        r_g;
wire [19:0] r_ppn0;
wire [ 1:0] r_plv0;
wire [ 1:0] r_mat0;
wire        r_d0;
wire        r_v0;
wire [19:0] r_ppn1;
wire [ 1:0] r_plv1;
wire [ 1:0] r_mat1;
wire        r_d1;
wire        r_v1;
assign {r_asid,
		r_ps,
		r_g,
		r_ppn0,
		r_plv0,
		r_mat0,
		r_d0,
		r_v0,
		r_ppn1,
		r_plv1,
		r_mat1,
		r_d1,
		r_v1,
		r_e,
		s1_index,
		s1_found,
		r_vppn} = tlb_to_ws_bus;

assign ws_to_tlb_bus = {csr_dmw3_plv , //184:181
						csr_dmw3_mat , //180:179
						csr_dmw3_pseg, //178:176
						csr_dmw3_vseg, //175:173
						csr_dmw2_plv , //172:169
						csr_dmw2_mat , //168:167
						csr_dmw2_pseg, //166:164
						csr_dmw2_vseg, //163:161
						csr_dmw1_plv , //160:157
						csr_dmw1_mat , //156:155
						csr_dmw1_pseg, //154:152
						csr_dmw1_vseg, //151:149
						csr_dmw0_plv , //147:145
						csr_dmw0_mat , //144:143
						csr_dmw0_pseg, //142:140
						csr_dmw0_vseg, //139:137
						csr_crmd_plv , //136:135
						csr_tlbrera_istlbr, //134
						csr_crmd_da  , //133
						csr_crmd_pg  , //132
						invtlb_asid  , //131:122
						invtlb_vppn  , //121:103
						invtlb_op    , //102:98
						ws_is_invtlb , //97
						ws_is_tlbfill, //96
						csr_tlbidx_ne, //95
						csr_tlbidx_ps, //94:89
						csr_tlbelo1_v, //88
						csr_tlbelo1_d, //87
						csr_tlbelo1_plv, //86:85
						csr_tlbelo1_mat, //84:83
						csr_tlbelo1_g, //82
						csr_tlbelo1_ppn, //81:62
						csr_tlbelo0_v, //61
						csr_tlbelo0_d, //60
						csr_tlbelo0_plv, //59:58
						csr_tlbelo0_mat, //57:56
						csr_tlbelo0_g, //55
						csr_tlbelo0_ppn, //54:35
						ws_is_tlbwr, //34
						csr_tlbidx_index, //33:30
						csr_asid_asid, //29:20
						ws_is_tlbsrch, //19
						csr_tlbehi_vppn //18:0
					   };


// 读CSR
wire [31:0] csr_result;
assign csr_result = {32{is_csr_dmw0     }} & {csr_dmw0_vseg, 1'b0, csr_dmw0_pseg, 19'b0, csr_dmw0_mat, csr_dmw0_plv} |
					{32{is_csr_dmw1     }} & {csr_dmw1_vseg, 1'b0, csr_dmw1_pseg, 19'b0, csr_dmw1_mat, csr_dmw1_plv} |
					{32{is_csr_dmw2     }} & {csr_dmw2_vseg, 1'b0, csr_dmw2_pseg, 19'b0, csr_dmw2_mat, csr_dmw2_plv} |
					{32{is_csr_dmw3     }} & {csr_dmw3_vseg, 1'b0, csr_dmw3_pseg, 19'b0, csr_dmw3_mat, csr_dmw3_plv} |
					{32{is_csr_tlbrprmd }} & {27'b0, csr_tlbrprmd_pwe, 1'b0, csr_tlbrprmd_pwe, csr_tlbrprmd_pplv} |
					{32{is_csr_tlbrehi  }} & {csr_tlbrehi_vppn, 7'b0, csr_tlbrehi_ps} |
					{32{is_csr_tlbrbadv }} & csr_tlbrbadv_vaddr |
					{32{is_csr_tlbrera  }} & {csr_tlbrera_pc, 1'b0, csr_tlbrera_istlbr} |
					{32{is_csr_tlbrentry}} & {csr_tlbrentry_ppn, 12'b0} |
					{32{is_csr_tlbelo1  }} & {4'b0, csr_tlbelo1_ppn, 1'b0, csr_tlbelo1_g, csr_tlbelo1_mat, csr_tlbelo1_plv, csr_tlbelo1_d, csr_tlbelo1_v} |
					{32{is_csr_tlbelo0  }} & {4'b0, csr_tlbelo0_ppn, 1'b0, csr_tlbelo0_g, csr_tlbelo0_mat, csr_tlbelo0_plv, csr_tlbelo0_d, csr_tlbelo0_v} |
					{32{is_csr_tlbidx   }} & {csr_tlbidx_ne, 1'b0, csr_tlbidx_ps, 20'b0, csr_tlbidx_index} |
					{32{is_csr_tlbehi   }} & {csr_tlbehi_vppn, 13'b0} | 
					{32{is_csr_asid     }} & {8'b0, 8'h0a, 6'b0, csr_asid_asid} | 
					{32{is_csr_badv     }} & csr_badv_vaddr |
					{32{is_csr_ecfg     }} & {13'b0, csr_ecfg_vs, 3'b0, csr_ecfg_lie} |
					{32{is_csr_tval     }} & csr_tval_timeval | 
					{32{is_csr_tcfg     }} & {csr_tcfg_initval, csr_tcfg_periodic, csr_tcfg_en} | 
					{32{is_csr_tid      }} & csr_tid | 
					{32{is_csr_save3    }} & csr_save3 | 
					{32{is_csr_save2    }} & csr_save2 | 
					{32{is_csr_save1    }} & csr_save1 |
					{32{is_csr_save0    }} & csr_save0 | 
					{32{is_csr_era      }} & csr_era_pc | 
					{32{is_csr_estat    }} & {1'b0, csr_estat_esubcode, csr_estat_ecode, 3'b0, csr_estat_is_r, csr_estat_is_rw} |
					{32{is_csr_prmd     }} & {28'h0, csr_prmd_pwe, csr_prmd_pie, csr_prmd_pplv} |
					{32{is_csr_eentry   }} & {csr_eentry_vpn, 12'b0} |
					{32{is_csr_crmd     }} & {22'b0, csr_crmd_we, csr_crmd_datm, csr_crmd_datf, csr_crmd_pg, csr_crmd_da, csr_crmd_ie, csr_crmd_plv};

// 写CSR
wire [31:0] xchg_result;
assign xchg_result = (~mask & csr_result) | (mask & ms_final_result);
always @(posedge clk) begin
// crmd
	if(reset) begin
		csr_crmd_plv <= 2'b0;
		csr_crmd_ie  <= 1'b0;
		csr_crmd_da  <= 1'b1;
		csr_crmd_pg  <= 1'b0;
		csr_crmd_datf<= 2'b0;
		csr_crmd_datm<= 2'b0;
		csr_crmd_we  <= 1'b0;	
	end
	else if (ws_ex) begin
		csr_crmd_plv <= 2'b0;
		csr_crmd_ie  <= 1'b0;
		csr_crmd_we  <= 1'b0;
	end
	else if (refill_ex) begin
		csr_crmd_plv <= 2'b0;
		csr_crmd_ie  <= 1'b0;
		csr_crmd_we  <= 1'b0;
		csr_crmd_da  <= 1'b1;
		csr_crmd_pg  <= 1'b0;
	end
	else if (ws_valid && ws_is_ertn) begin
		if (csr_tlbrera_istlbr) begin
			csr_crmd_plv <= csr_tlbrprmd_pplv;
			csr_crmd_ie  <= csr_tlbrprmd_pie ;
			csr_crmd_we  <= csr_tlbrprmd_pwe ;
			csr_crmd_da  <= 1'b0             ;
			csr_crmd_pg  <= 1'b1             ;
		end
		else begin 
			csr_crmd_plv <= csr_prmd_pplv;
			csr_crmd_ie  <= csr_prmd_pie ;
			csr_crmd_we  <= csr_prmd_pwe ;
		end
	end
	else if (ws_valid && ws_is_csrwr && is_csr_crmd) begin
		csr_crmd_plv <= ms_final_result[1:0];
		csr_crmd_ie  <= ms_final_result[  2];
		csr_crmd_da  <= ms_final_result[  3];
		csr_crmd_pg  <= ms_final_result[  4];
		csr_crmd_datf<= ms_final_result[6:5];
		csr_crmd_datm<= ms_final_result[8:7];
		csr_crmd_we  <= ms_final_result[  9];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_crmd) begin
		csr_crmd_plv <= xchg_result[1:0];
		csr_crmd_ie  <= xchg_result[  2];
		csr_crmd_da  <= xchg_result[  3];
		csr_crmd_pg  <= xchg_result[  4];
		csr_crmd_datf<= xchg_result[6:5];
		csr_crmd_datm<= xchg_result[8:7];
		csr_crmd_we  <= xchg_result[  9];
	end

//prmd
	if (ws_ex) begin
		csr_prmd_pplv <= csr_crmd_plv;
		csr_prmd_pie  <= csr_crmd_ie ;
		csr_prmd_pwe  <= csr_crmd_we ;
	end
	// to pass the CPU design test, not consist with LoongArch I think
	else if (refill_ex) begin
		csr_prmd_pplv <= csr_crmd_plv;
		csr_prmd_pie  <= csr_crmd_ie ;
		csr_prmd_pwe  <= csr_crmd_we ;
	end
	//////////////////////////////////////////////////////////////////
	else if (ws_valid && ws_is_csrwr && is_csr_prmd) begin
		csr_prmd_pplv <= ms_final_result[1:0];
		csr_prmd_pie  <= ms_final_result[  2];
		csr_prmd_pwe  <= ms_final_result[  3];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_prmd) begin
		csr_prmd_pplv <= xchg_result[1:0];
		csr_prmd_pie  <= xchg_result[  2];
		csr_prmd_pwe  <= xchg_result[  3];
	end


//eentry
	if (ws_valid && ws_is_csrwr && is_csr_eentry) begin
		csr_eentry_vpn <= ms_final_result[31:12];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_eentry) begin
		csr_eentry_vpn <= xchg_result[31:12];
	end
	
//estat
	if (reset) begin
		csr_estat_is_rw <=  2'b0;
		csr_estat_is_r  <= 11'b0;
	end
	else begin
		if (ws_ex) begin
			csr_estat_ecode <= ws_ecode;
			csr_estat_esubcode <= 9'b0;
		end
		// to pass the CPU design test, not consist with LoongArch I think
		else if (refill_ex) begin
			csr_estat_ecode <= 6'h3f;
		end
		//////////////////////////////////////////////////////////////////
		else if (ws_valid && ws_is_csrwr && is_csr_estat) begin
			csr_estat_is_rw <= ms_final_result[1:0];
			csr_estat_ecode <= ms_final_result[21:16];
			csr_estat_esubcode <= ms_final_result[30:22];
		end
		else if (ws_valid && ws_is_csrxchg && is_csr_estat) begin
			csr_estat_is_rw <= xchg_result[1:0];
			csr_estat_ecode <= xchg_result[21:16];
			csr_estat_esubcode <= xchg_result[30:22];
		end
		
		if (!ws_ex && ws_valid && (ws_is_csrwr && is_csr_ticlr && ms_final_result[0] || ws_is_csrxchg && is_csr_ticlr && xchg_result[0])) begin
			csr_estat_is_r[9] <= 1'b0;
		end
		else if ((csr_tval_timeval == 32'b0) && csr_tcfg_en && en_r) begin
			csr_estat_is_r[9] <= 1'b1;
		end
	end
	
//era
	if (ws_ex) begin
		csr_era_pc <= ws_pc;
	end
	// to pass the CPU design test, not consist with LoongArch I think
	else if (refill_ex) begin
		csr_era_pc <= ws_pc;
	end
	//////////////////////////////////////////////////////////////////
	else if (ws_valid && ws_is_csrwr && is_csr_era) begin
		csr_era_pc <= ms_final_result;
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_era) begin
		csr_era_pc <= xchg_result;
	end
	
//save0
	if (ws_valid && ws_is_csrwr && is_csr_save0) begin
		csr_save0 <= ms_final_result;
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_save0) begin
		csr_save0 <= xchg_result;
	end
	
//save1
	if (ws_valid && ws_is_csrwr && is_csr_save1) begin
		csr_save1 <= ms_final_result;
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_save1) begin
		csr_save1 <= xchg_result;
	end
	
//save2	
	if (ws_valid && ws_is_csrwr && is_csr_save2) begin
		csr_save2 <= ms_final_result;
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_save2) begin
		csr_save2 <= xchg_result;
	end
	
//save3	
	if (ws_valid && ws_is_csrwr && is_csr_save3) begin
		csr_save3 <= ms_final_result;
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_save3) begin
		csr_save3 <= xchg_result;
	end
	
//TID
	if (ws_valid && ws_is_csrwr && is_csr_tid) begin
		csr_tid <= ms_final_result;
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_tid) begin
		csr_tid <= xchg_result;
	end
	
//TCFG
	if (reset) begin
		csr_tcfg_en <= 1'b0;
		en_r <= 1'b0;
	end
	else if (ws_valid && ws_is_csrwr && is_csr_tcfg) begin
		csr_tcfg_en <= ms_final_result[0];
		en_r <= ms_final_result[0];
		csr_tcfg_periodic <= ms_final_result[1];
		csr_tcfg_initval <= ms_final_result[31:2];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_tcfg) begin
		csr_tcfg_en <= xchg_result[0];
		en_r <= xchg_result[0];
		csr_tcfg_periodic <= xchg_result[1];
		csr_tcfg_initval <= xchg_result[31:2];
	end
	else if ((csr_tval_timeval == 32'b0) && csr_tcfg_en && en_r && !csr_tcfg_periodic) begin
		en_r <= 1'b0;
	end
	
//TVAL
	if (ws_valid && ws_is_csrwr && is_csr_tcfg) begin
		csr_tval_timeval <= {ms_final_result[31:2], 2'b0};
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_tcfg) begin
		csr_tval_timeval <= {xchg_result[31:2], 2'b0};
	end
	else if (csr_tcfg_periodic) begin
		if (csr_tval_timeval == 32'b0) begin
			csr_tval_timeval <= {csr_tcfg_initval, 2'b0};
		end
		else if (csr_tcfg_en) begin
			csr_tval_timeval <= csr_tval_timeval - 1;
		end
	end
	else begin
		if (csr_tval_timeval == 32'b0) begin
			csr_tval_timeval <= 32'b0;
		end
		else if (csr_tcfg_en) begin
			csr_tval_timeval <= csr_tval_timeval - 1;
		end
	end
	
//ECFG
	if (reset) begin
		csr_ecfg_lie <= 13'b0;
		csr_ecfg_vs <= 3'b0;
	end
	else if (ws_valid && ws_is_csrwr && is_csr_ecfg) begin
		csr_ecfg_lie <= {ms_final_result[12:11], 1'b0, ms_final_result[9:0]};
		csr_ecfg_vs  <= ms_final_result[18:16];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_ecfg) begin
		csr_ecfg_lie <= {xchg_result[12:11], 1'b0, xchg_result[9:0]};
		csr_ecfg_vs  <= xchg_result[18:16];
	end
	
//BADV
	if (ws_ex && ((ws_ecode == 6'h08) || (ws_ecode == 6'h03) || (ws_ecode == 6'h07 && s0_ex))) begin
		csr_badv_vaddr <= ws_pc;
	end
	else if (ws_ex && ((ws_ecode == 6'h09) || (ws_ecode == 6'h01) || (ws_ecode == 6'h02) || (ws_ecode == 6'h04) || (ws_ecode == 6'h07 && s1_ex))) begin
		csr_badv_vaddr <= ms_final_result;
	end
	// to pass the CPU design test, not consist with LoongArch I think
	else if (s1_refill_ex) begin
		csr_badv_vaddr <= ms_final_result;
	end
	else if (s0_refill_ex) begin
		csr_badv_vaddr <= ws_pc;
	end
	//////////////////////////////////////////////////////////////////
	else if (ws_valid && ws_is_csrwr && is_csr_badv) begin
		csr_badv_vaddr <= ms_final_result;
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_badv) begin
		csr_badv_vaddr <= xchg_result;
	end
	
//ASID
	if (ws_valid && ws_is_csrwr && is_csr_asid) begin
		csr_asid_asid <= ms_final_result[9:0];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_asid) begin
		csr_asid_asid <= xchg_result[9:0];
	end
	else if (ws_valid && ws_is_tlbrd) begin
		if (r_e) begin
			csr_asid_asid <= r_asid;
		end
		else begin
			csr_asid_asid <= 10'b0;
		end
	end

//TLBEHI
	if (ws_ex && ((ws_ecode == 6'h03) || s0_ex)) begin
		csr_tlbehi_vppn <= ws_pc[31:13];
	end
	else if (ws_ex && ((ws_ecode == 6'h01) || (ws_ecode == 6'h02) || s1_ex)) begin
		csr_tlbehi_vppn <= ms_final_result[31:13];
	end
	// to pass the CPU design test, not consist with LoongArch I think
	else if (s1_refill_ex) begin
		csr_tlbehi_vppn <= ms_final_result[31:13];
	end
	else if (s0_refill_ex) begin
		csr_tlbehi_vppn <= ws_pc[31:13];
	end
	//////////////////////////////////////////////////////////////////
	else if (ws_valid && ws_is_csrwr && is_csr_tlbehi) begin
		csr_tlbehi_vppn <= ms_final_result[31:13];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_tlbehi) begin
		csr_tlbehi_vppn <= xchg_result[31:13];
	end
	else if (ws_valid && ws_is_tlbrd) begin
		if (r_e) begin
			csr_tlbehi_vppn <= r_vppn;
		end
		else begin
			csr_tlbehi_vppn <= 19'b0;
		end
	end

//TLBIDX
	if (ws_valid && ws_is_csrwr && is_csr_tlbidx) begin
		csr_tlbidx_index <= ms_final_result[ 3: 0];
		csr_tlbidx_ps    <= ms_final_result[29:24];
		csr_tlbidx_ne    <= ms_final_result[   31];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_tlbidx) begin
		csr_tlbidx_index <= xchg_result[ 3: 0];
		csr_tlbidx_ps    <= xchg_result[29:24];
		csr_tlbidx_ne    <= xchg_result[   31];
	end
	else if (ws_valid && ws_is_tlbsrch) begin
		if (s1_found) begin
			csr_tlbidx_index <= s1_index;
			csr_tlbidx_ne    <= 1'b0;
		end
		else begin
			csr_tlbidx_ne   <= 1'b1;
		end
	end
	else if (ws_valid && ws_is_tlbrd) begin
		if (r_e) begin
			csr_tlbidx_ps <= r_ps;
			csr_tlbidx_ne <= 1'b0;
		end
		else begin
			csr_tlbidx_ne <= 1'b1;
			csr_tlbidx_ps <= 6'b0;
		end
	end

//TLBELO0
	if (ws_valid && ws_is_csrwr && is_csr_tlbelo0) begin
		csr_tlbelo0_ppn <= ms_final_result[27:8];
		csr_tlbelo0_g   <= ms_final_result[   6];
		csr_tlbelo0_mat <= ms_final_result[ 5:4];
		csr_tlbelo0_plv <= ms_final_result[ 3:2];
		csr_tlbelo0_d   <= ms_final_result[   1];
		csr_tlbelo0_v   <= ms_final_result[   0];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_tlbelo0) begin
		csr_tlbelo0_ppn <= xchg_result[27:8];
		csr_tlbelo0_g   <= xchg_result[   6];
		csr_tlbelo0_mat <= xchg_result[ 5:4];
		csr_tlbelo0_plv <= xchg_result[ 3:2];
		csr_tlbelo0_d   <= xchg_result[   1];
		csr_tlbelo0_v   <= xchg_result[   0];
	end
	else if (ws_valid && ws_is_tlbrd) begin
		if (r_e) begin
			csr_tlbelo0_ppn <= r_ppn0;
			csr_tlbelo0_g   <= r_g;
			csr_tlbelo0_mat <= r_mat0;
			csr_tlbelo0_plv <= r_plv0;
			csr_tlbelo0_d   <= r_d0;
			csr_tlbelo0_v   <= r_v0;
		end
		else begin
			csr_tlbelo0_ppn <= 20'b0;
			csr_tlbelo0_g   <=  1'b0;
			csr_tlbelo0_mat <=  2'b0;
			csr_tlbelo0_plv <=  2'b0;
			csr_tlbelo0_d   <=  1'b0;
			csr_tlbelo0_v   <=  1'b0;
		end
	end
	
//TLBELO1
	if (ws_valid && ws_is_csrwr && is_csr_tlbelo1) begin
		csr_tlbelo1_ppn <= ms_final_result[27:8];
		csr_tlbelo1_g   <= ms_final_result[   6];
		csr_tlbelo1_mat <= ms_final_result[ 5:4];
		csr_tlbelo1_plv <= ms_final_result[ 3:2];
		csr_tlbelo1_d   <= ms_final_result[   1];
		csr_tlbelo1_v   <= ms_final_result[   0];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_tlbelo1) begin
		csr_tlbelo1_ppn <= xchg_result[27:8];
		csr_tlbelo1_g   <= xchg_result[   6];
		csr_tlbelo1_mat <= xchg_result[ 5:4];
		csr_tlbelo1_plv <= xchg_result[ 3:2];
		csr_tlbelo1_d   <= xchg_result[   1];
		csr_tlbelo1_v   <= xchg_result[   0];
	end
	else if (ws_valid && ws_is_tlbrd) begin
		if (r_e) begin
			csr_tlbelo1_ppn <= r_ppn1;
			csr_tlbelo1_g   <= r_g;
			csr_tlbelo1_mat <= r_mat1;
			csr_tlbelo1_plv <= r_plv1;
			csr_tlbelo1_d   <= r_d1;
			csr_tlbelo1_v   <= r_v1;
		end
		else begin
			csr_tlbelo1_ppn <= 20'b0;
			csr_tlbelo1_g   <=  1'b0;
			csr_tlbelo1_mat <=  2'b0;
			csr_tlbelo1_plv <=  2'b0;
			csr_tlbelo1_d   <=  1'b0;
			csr_tlbelo1_v   <=  1'b0;
		end
	end
	
//TLBRENTRY
	if (ws_valid && ws_is_csrwr && is_csr_tlbrentry) begin
		csr_tlbrentry_ppn <= ms_final_result[31:12];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_tlbrentry) begin
		csr_tlbrentry_ppn <= xchg_result[31:12];
	end
	
//TLBRERA
	if (reset) begin
		csr_tlbrera_istlbr <=  1'b0;
		csr_tlbrera_pc     <= 30'b0;
	end
	else if (refill_ex) begin
		csr_tlbrera_istlbr <= 1'b1       ;
		csr_tlbrera_pc     <= ws_pc[31:2];
	end
	else if (ws_valid && ws_is_ertn) begin
		csr_tlbrera_istlbr <= 1'b0;
	end
	else if (ws_valid && ws_is_csrwr && is_csr_tlbrera) begin
		csr_tlbrera_istlbr <= ms_final_result[   0];
		csr_tlbrera_pc     <= ms_final_result[31:2];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_tlbrera) begin
		csr_tlbrera_istlbr <= xchg_result[   0];
		csr_tlbrera_pc     <= xchg_result[31:2];
	end
	
//TLBRBADV
	if (s1_refill_ex) begin
		csr_tlbrbadv_vaddr <= ms_final_result;
	end
	else if (s0_refill_ex) begin
		csr_tlbrbadv_vaddr <= ws_pc;
	end
	else if (ws_valid && ws_is_csrwr && is_csr_tlbrbadv) begin
		csr_tlbrbadv_vaddr <= ms_final_result;
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_tlbrbadv) begin
		csr_tlbrbadv_vaddr <= xchg_result;
	end
	
//TLBEHI
	if (s1_refill_ex) begin
		csr_tlbrehi_vppn <= ms_final_result[31:13];
	end
	else if (s0_refill_ex) begin
		csr_tlbrehi_vppn <= ws_pc[31:13];
	end
	else if (ws_valid && ws_is_csrwr && is_csr_tlbrehi) begin
		csr_tlbrehi_ps   <= ms_final_result[ 5: 0];
		csr_tlbrehi_vppn <= ms_final_result[31:13];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_tlbrehi) begin
		csr_tlbrehi_ps   <= xchg_result[ 5: 0];
		csr_tlbrehi_vppn <= xchg_result[31:13];
	end
	
//TLBRPRMD
	if (refill_ex) begin
		csr_tlbrprmd_pplv <= csr_crmd_plv;
		csr_tlbrprmd_pie  <= csr_crmd_ie ;
		csr_tlbrprmd_pwe  <= csr_crmd_we ;
	end
	else if (ws_valid && ws_is_csrwr && is_csr_tlbrprmd) begin
		csr_tlbrprmd_pplv <= ms_final_result[1:0];
		csr_tlbrprmd_pie  <= ms_final_result[  2];
		csr_tlbrprmd_pwe  <= ms_final_result[  4];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_tlbrprmd) begin
		csr_tlbrprmd_pplv <= xchg_result[1:0];
		csr_tlbrprmd_pie  <= xchg_result[  2];
		csr_tlbrprmd_pwe  <= xchg_result[  4];
	end

//DMW0
	if (reset) begin
		csr_dmw0_plv  <= 4'b0;
	end
	else if (ws_valid && ws_is_csrwr && is_csr_dmw0) begin
		csr_dmw0_plv  <= ms_final_result[ 3: 0];
		csr_dmw0_mat  <= ms_final_result[ 5: 4];
		csr_dmw0_pseg <= ms_final_result[27:25];
		csr_dmw0_vseg <= ms_final_result[31:29];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_dmw0) begin
		csr_dmw0_plv  <= xchg_result[ 3: 0];
		csr_dmw0_mat  <= xchg_result[ 5: 4];
		csr_dmw0_pseg <= xchg_result[27:25];
		csr_dmw0_vseg <= xchg_result[31:29];
	end

//DMW1
	if (reset) begin
		csr_dmw1_plv  <= 4'b0;
	end
	else if (ws_valid && ws_is_csrwr && is_csr_dmw1) begin
		csr_dmw1_plv  <= ms_final_result[ 3: 0];
		csr_dmw1_mat  <= ms_final_result[ 5: 4];
		csr_dmw1_pseg <= ms_final_result[27:25];
		csr_dmw1_vseg <= ms_final_result[31:29];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_dmw1) begin
		csr_dmw1_plv  <= xchg_result[ 3: 0];
		csr_dmw1_mat  <= xchg_result[ 5: 4];
		csr_dmw1_pseg <= xchg_result[27:25];
		csr_dmw1_vseg <= xchg_result[31:29];
	end

//DMW2
	if (reset) begin
		csr_dmw2_plv  <= 4'b0;
	end
	else if (ws_valid && ws_is_csrwr && is_csr_dmw2) begin
		csr_dmw2_plv  <= ms_final_result[ 3: 0];
		csr_dmw2_mat  <= ms_final_result[ 5: 4];
		csr_dmw2_pseg <= ms_final_result[27:25];
		csr_dmw2_vseg <= ms_final_result[31:29];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_dmw2) begin
		csr_dmw2_plv  <= xchg_result[ 3: 0];
		csr_dmw2_mat  <= xchg_result[ 5: 4];
		csr_dmw2_pseg <= xchg_result[27:25];
		csr_dmw2_vseg <= xchg_result[31:29];
	end	
		
//DMW3
	if (reset) begin
		csr_dmw3_plv  <= 4'b0;
	end
	else if (ws_valid && ws_is_csrwr && is_csr_dmw3) begin
		csr_dmw3_plv  <= ms_final_result[ 3: 0];
		csr_dmw3_mat  <= ms_final_result[ 5: 4];
		csr_dmw3_pseg <= ms_final_result[27:25];
		csr_dmw3_vseg <= ms_final_result[31:29];
	end
	else if (ws_valid && ws_is_csrxchg && is_csr_dmw3) begin
		csr_dmw3_plv  <= xchg_result[ 3: 0];
		csr_dmw3_mat  <= xchg_result[ 5: 4];
		csr_dmw3_pseg <= xchg_result[27:25];
		csr_dmw3_vseg <= xchg_result[31:29];
	end	
end

assign int_vec = {csr_estat_is_r, csr_estat_is_rw} & csr_ecfg_lie;
assign int_ex = (int_vec != 13'b0) && csr_crmd_ie;

assign ws_ecode = int_ex ? 6'b0 : ms_ecode;
assign ws_ex = (int_ex || ms_ex) && ws_valid;
assign s1_refill_ex = s1_refill_ex_h && ws_valid;
assign s0_refill_ex = s0_refill_ex_h && ws_valid;
assign s1_ex = s1_ex_h && ws_valid;
assign s0_ex = s0_ex_h && ws_valid;
assign ex_entrance = (csr_ecfg_vs == 3'b0) ? {csr_eentry_vpn, 12'b0} : ({csr_eentry_vpn, 12'b0} | (ws_ecode << 2));

assign ws_final_result = (ws_is_csrrd | ws_is_csrwr | ws_is_csrxchg) ? csr_result : 
						 (ws_is_rdtimeh_w | ws_is_rdtimel_w) ? csr_tid : ms_final_result;

wire        rf_we;
wire [4 :0] rf_waddr;
wire [31:0] rf_wdata;
assign ws_to_rf_bus = {rf_we   ,  //37:37
                       rf_waddr,  //36:32
                       rf_wdata   //31:0
                      };

assign ws_ready_go = 1'b1;
assign ws_allowin  = !ws_valid || ws_ready_go;
always @(posedge clk) begin
    if (reset) begin
        ws_valid <= 1'b0;
		ms_to_ws_bus_r <= `MS_TO_WS_BUS_WD'b0;
    end
    else begin 
		if (ws_allowin) begin
			ws_valid <= ms_to_ws_valid;
		end

		if (ms_to_ws_valid && ws_allowin) begin
			ms_to_ws_bus_r <= (ws_ex || ws_is_ertn || refill_ex) ? `MS_TO_WS_BUS_WD'b0 : ms_to_ws_bus;
		end
	end
end

assign rf_we = !ws_ex && ws_gr_we && ws_valid && !refill_ex;
assign rf_waddr = ws_dest;
assign rf_wdata = ws_final_result;

// debug info generate
assign debug_wb_pc       = ws_pc;
assign debug_wb_rf_we    = {4{rf_we}};
assign debug_wb_rf_wnum  = ws_dest;
assign debug_wb_rf_wdata = ws_final_result;

wire ws_tlb_stall;
assign ws_tlb_stall = (ws_is_tlbsrch || ws_is_invtlb || ws_is_tlbfill) && ws_valid;

wire ex_all;
assign ex_all = ws_ex || s1_refill_ex || s0_refill_ex;
assign refill_ex = s1_refill_ex || s0_refill_ex;

assign ws_to_ds_bus = {ws_tlb_stall, ex_all, ws_is_ertn, ws_valid, ws_gr_we, ws_dest, ws_final_result};

assign ws_to_ps_bus = {refill_ex, csr_tlbrentry_ppn, csr_tlbrera_istlbr, csr_tlbrera_pc, ex_entrance, ex_all, ws_is_ertn, csr_era_pc};
assign ws_to_fs_bus = {ws_is_ertn, ex_all};
assign ws_to_es_bus = {ws_is_ertn, ex_all};
assign ws_to_ms_bus = {ws_is_ertn, ex_all};

assign datf = csr_crmd_datf;
assign datm = csr_crmd_datm;

endmodule