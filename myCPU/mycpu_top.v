//LoongArch
`include "mycpu.h"

//记得改mycpu_top，跑chiplab用core_top
module mycpu_top(
    input  wire [7:0] ext_int,
    //chiplab
    input wire [7:0] intrpt,
    input wire break_point,
    input wire infor_flag,
    input wire [4:0] reg_num,

    input  wire       aclk,
    input  wire       aresetn,
	// axi interface
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

	input  wire[3 :0]  rid   ,
	input  wire[31:0]  rdata ,
	input  wire[1 :0]  rresp ,
	input  wire        rlast ,
	input  wire        rvalid,
	output wire        rready,

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
	
	input  wire[3 :0]  bid   ,
	input  wire[1 :0]  bresp ,
	input  wire        bvalid,
	output wire        bready,
    // trace debug interface
    output wire[31:0] debug_wb_pc      ,
    output wire[ 3:0] debug_wb_rf_we   ,
    output wire[ 4:0] debug_wb_rf_wnum ,
    output wire[31:0] debug_wb_rf_wdata
);
`ifdef DIFFTEST_EN
wire [31:0] regs[31:0];
`endif

wire fs_is_icacop, ds_is_icacop, rs_is_icacop;
wire rs_is_cacop, rs_is_dcacop;
wire [4:0] cacop_code;
wire [1:0] cacop_op;
wire is_icacop = cacop_code[2:0] == 3'b0;
wire is_dcacop = cacop_code[2:0] == 3'b1;
assign cacop_op = cacop_code[4:3];
assign rs_is_dcacop = rs_is_cacop && !rs_is_icacop;

wire clk, resetn;
assign clk = aclk;
assign resetn = aresetn;

//pre-IF_stage
wire [31:0] ps_inst_addr;
wire ps_inst_req;
// pre-MEM_stage
wire [31:0] rs_data_addr;
wire rs_data_req;

reg         reset;
always @(posedge clk) reset <= ~resetn;

wire         fs_allowin;
wire         ds_allowin;
wire         es_allowin;
wire         rs_allowin;
wire         ms_allowin;
wire         ws_allowin;
wire         ps_to_fs_valid;
wire         fs_to_ds_valid;
wire         ds_to_es_valid;
wire         es_to_rs_valid;
wire         rs_to_ms_valid;
wire         ms_to_ws_valid;
wire [`PS_TO_FS_BUS_WD -1:0] ps_to_fs_bus;
wire [`FS_TO_DS_BUS_WD -1:0] fs_to_ds_bus;
wire [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus;
wire [`ES_TO_RS_BUS_WD -1:0] es_to_rs_bus;
wire [`RS_TO_MS_BUS_WD -1:0] rs_to_ms_bus;
wire [`MS_TO_WS_BUS_WD -1:0] ms_to_ws_bus;
wire [`WS_TO_RF_BUS_WD -1:0] ws_to_rf_bus;
wire [`BR_BUS_WD       -1:0] br_bus;

wire es_br_taken;

// forward
wire [`ES_TO_DS_BUS_WD - 1:0] es_to_ds_bus;
wire [`RS_TO_DS_BUS_WD - 1:0] rs_to_ds_bus;
wire [`MS_TO_DS_BUS_WD - 1:0] ms_to_ds_bus;
wire [`WS_TO_DS_BUS_WD - 1:0] ws_to_ds_bus;

wire [`WS_TO_PS_BUS_WD - 1:0] ws_to_ps_bus;
wire [`WS_TO_FS_BUS_WD - 1:0] ws_to_fs_bus;
wire [`WS_TO_TLB_BUS_WD -1:0] ws_to_tlb_bus;
wire [`TLB_TO_WS_BUS_WD -1:0] tlb_to_ws_bus;
wire [`TLB_TO_PS_BUS_WD -1:0] tlb_to_ps_bus;
wire [`TLB_TO_ES_BUS_WD -1:0] tlb_to_es_bus;
wire [`TLB_TO_RS_BUS_WD -1:0] tlb_to_rs_bus;
wire [1:0] ws_to_es_bus;
wire [1:0] ws_to_rs_bus;
wire [1:0] ws_to_ms_bus;
wire       ds_to_fs_bus;

wire ms_to_rs_bus;

wire wrongPC_br, wrong_req_r, br_bus_r_valid;

// search port 0 (for fetch)
wire [               18:0] s0_vppn    ;
wire                       s0_va_bit12;
wire [                9:0] s0_asid    ;
wire                       s0_found   ;
wire [$clog2(`TLBNUM)-1:0] s0_index   ; // 4位索引，[3:0]
wire [               19:0] s0_ppn     ;
wire [                5:0] s0_ps      ;
wire [                1:0] s0_plv     ;
wire [                1:0] s0_mat     ;
wire                       s0_d       ;
wire                       s0_v       ;
// search port 1 (for load/store)
wire [               18:0] s1_vppn    ;
wire                       s1_va_bit12;
wire [                9:0] s1_asid    ;
wire                       s1_found   ;
wire [$clog2(`TLBNUM)-1:0] s1_index   ;
wire [               19:0] s1_ppn     ;
wire [                5:0] s1_ps      ;
wire [                1:0] s1_plv     ;
wire [                1:0] s1_mat     ;
wire                       s1_d       ;
wire                       s1_v       ;
// invtlb opcode
wire                      invtlb_valid;
wire [               4:0] invtlb_op   ;
// write port
wire                       we     ;
wire [$clog2(`TLBNUM)-1:0] w_index;
wire                       w_e    ;
wire [               18:0] w_vppn ;
wire [                5:0] w_ps   ;
wire [                9:0] w_asid ;
wire                       w_g    ;
wire [               19:0] w_ppn0 ;
wire [                1:0] w_plv0 ;
wire [                1:0] w_mat0 ;
wire                       w_d0   ;
wire                       w_v0   ;
wire [               19:0] w_ppn1 ;
wire [                1:0] w_plv1 ;
wire [                1:0] w_mat1 ;
wire                       w_d1   ;
wire                       w_v1   ;
// read port
wire [$clog2(`TLBNUM)-1:0] r_index;
wire                       r_e    ;
wire [               18:0] r_vppn ;
wire [                5:0] r_ps   ;
wire [                9:0] r_asid ;
wire                       r_g    ;
wire [               19:0] r_ppn0 ;
wire [                1:0] r_plv0 ;
wire [                1:0] r_mat0 ;
wire                       r_d0   ;
wire                       r_v0   ;
wire [               19:0] r_ppn1 ;
wire [                1:0] r_plv1 ;
wire [                1:0] r_mat1 ;
wire                       r_d1   ;
wire                       r_v1   ;

// ws_to_tlb_bus;
wire                       csr_tlbrera_istlbr;
wire                       csr_crmd_pg       ;
wire                       csr_crmd_da       ;
wire [               18:0] csr_tlbehi_vppn   ;
wire [                9:0] csr_asid_asid     ;
wire [                3:0] csr_tlbidx_index  ;
wire [                5:0] csr_tlbidx_ps     ;
wire                       csr_tlbidx_ne     ;
wire                       ws_is_tlbwr       ;
wire                       ws_is_tlbsrch     ;
wire                       ws_is_tlbfill     ;
wire                       csr_tlbelo0_v     ;
wire                       csr_tlbelo0_d     ;
wire [                1:0] csr_tlbelo0_plv   ;
wire [                1:0] csr_tlbelo0_mat   ;
wire                       csr_tlbelo0_g     ;
wire [               19:0] csr_tlbelo0_ppn   ;
wire                       csr_tlbelo1_v     ;
wire                       csr_tlbelo1_d     ;
wire [                1:0] csr_tlbelo1_plv   ;
wire [                1:0] csr_tlbelo1_mat   ;
wire                       csr_tlbelo1_g     ;
wire [               19:0] csr_tlbelo1_ppn   ;
wire                       ws_is_invtlb      ;
wire [               18:0] invtlb_vppn       ;
wire [                9:0] invtlb_asid       ;
wire [                1:0] csr_crmd_plv      ;

`ifdef DIFFTEST_EN
// from wb_stage
wire    [31:0]  debug_wb_inst       ;
wire            ws_valid_diff       ;
wire            cnt_inst_diff       ;
wire    [63:0]  timer_64_diff       ;
wire    [ 7:0]  inst_ld_en_diff     ;
wire    [31:0]  ld_paddr_diff       ;
wire    [31:0]  ld_vaddr_diff       ;
wire    [ 7:0]  inst_st_en_diff     ;
wire    [31:0]  st_paddr_diff       ;
wire    [31:0]  st_vaddr_diff       ;
wire    [31:0]  st_data_diff        ;
wire            csr_rstat_en_diff   ;
wire    [31:0]  csr_data_diff       ;

wire inst_valid_diff = ws_valid_diff;
reg             cmt_valid           ;
reg             cmt_cnt_inst        ;
reg     [63:0]  cmt_timer_64        ;
reg     [ 7:0]  cmt_inst_ld_en      ;
reg     [31:0]  cmt_ld_paddr        ;
reg     [31:0]  cmt_ld_vaddr        ;
reg     [ 7:0]  cmt_inst_st_en      ;
reg     [31:0]  cmt_st_paddr        ;
reg     [31:0]  cmt_st_vaddr        ;
reg     [31:0]  cmt_st_data         ;
reg             cmt_csr_rstat_en    ;
reg     [31:0]  cmt_csr_data        ;

reg             cmt_wen             ;
reg     [ 7:0]  cmt_wdest           ;
reg     [31:0]  cmt_wdata           ;
reg     [31:0]  cmt_pc              ;
reg     [31:0]  cmt_inst            ;

reg             cmt_excp_flush      ;
reg             cmt_ertn            ;
reg     [5:0]   cmt_csr_ecode       ;
reg             cmt_tlbfill_en      ;
reg     [4:0]   cmt_rand_index      ;

// to difftest debug
reg             trap                ;
reg     [ 7:0]  trap_code           ;
reg     [63:0]  cycleCnt            ;
reg     [63:0]  instrCnt            ;

// from regfile
wire    [31:0]  regs[31:0]          ;

// from csr
wire    [31:0]  csr_crmd_diff_0     ;
wire    [31:0]  csr_prmd_diff_0     ;
wire    [31:0]  csr_ectl_diff_0     ;
wire    [31:0]  csr_estat_diff_0    ;
wire    [31:0]  csr_era_diff_0      ;
wire    [31:0]  csr_badv_diff_0     ;
wire	[31:0]  csr_eentry_diff_0   ;
wire 	[31:0]  csr_tlbidx_diff_0   ;
wire 	[31:0]  csr_tlbehi_diff_0   ;
wire 	[31:0]  csr_tlbelo0_diff_0  ;
wire 	[31:0]  csr_tlbelo1_diff_0  ;
wire 	[31:0]  csr_asid_diff_0     ;
wire 	[31:0]  csr_save0_diff_0    ;
wire 	[31:0]  csr_save1_diff_0    ;
wire 	[31:0]  csr_save2_diff_0    ;
wire 	[31:0]  csr_save3_diff_0    ;
wire 	[31:0]  csr_tid_diff_0      ;
wire 	[31:0]  csr_tcfg_diff_0     ;
wire 	[31:0]  csr_tval_diff_0     ;
wire 	[31:0]  csr_ticlr_diff_0    ;
wire 	[31:0]  csr_llbctl_diff_0   ;
wire 	[31:0]  csr_tlbrentry_diff_0;
wire 	[31:0]  csr_dmw0_diff_0     ;
wire 	[31:0]  csr_dmw1_diff_0     ;
wire 	[31:0]  csr_pgdl_diff_0     ;
wire 	[31:0]  csr_pgdh_diff_0     ;

wire [31:0] debug0_wb_pc;
assign debug0_wb_pc = debug_wb_pc;
`endif

//dmw
wire [3:0] csr_dmw3_plv, csr_dmw2_plv, csr_dmw1_plv, csr_dmw0_plv;
wire [1:0] csr_dmw3_mat, csr_dmw2_mat, csr_dmw1_mat, csr_dmw0_mat;
wire [1:0] s0_hit_d_mat, s1_hit_d_mat;
wire [2:0] csr_dmw3_pseg, csr_dmw3_vseg, csr_dmw2_pseg, csr_dmw2_vseg, csr_dmw1_pseg, csr_dmw1_vseg, csr_dmw0_pseg, csr_dmw0_vseg;
wire s0_hit_d0, s0_hit_d1, s0_hit_d2, s0_hit_d3, s1_hit_d0, s1_hit_d1, s1_hit_d2, s1_hit_d3, s0_hit_d, s1_hit_d;
wire [2:0] dmw_res0, dmw_res1;
assign s0_hit_d0 = (ps_inst_addr[31:29] == csr_dmw0_vseg && csr_dmw0_plv[csr_crmd_plv]);
assign s0_hit_d1 = (ps_inst_addr[31:29] == csr_dmw1_vseg && csr_dmw1_plv[csr_crmd_plv]);
assign s0_hit_d2 = (ps_inst_addr[31:29] == csr_dmw2_vseg && csr_dmw2_plv[csr_crmd_plv]);
assign s0_hit_d3 = (ps_inst_addr[31:29] == csr_dmw3_vseg && csr_dmw3_plv[csr_crmd_plv]);
assign s0_hit_d_mat = ({2{s0_hit_d0}} & csr_dmw0_mat) | ({2{s0_hit_d1}} & csr_dmw1_mat) | ({2{s0_hit_d2}} & csr_dmw2_mat) | ({2{s0_hit_d3}} & csr_dmw3_mat);
assign dmw_res0 = ({3{s0_hit_d0}} & csr_dmw0_pseg) | ({3{s0_hit_d1}} & csr_dmw1_pseg) | ({3{s0_hit_d2}} & csr_dmw2_pseg) | ({3{s0_hit_d3}} & csr_dmw3_pseg);
assign s0_hit_d = s0_hit_d0 | s0_hit_d1 | s0_hit_d2 | s0_hit_d3;
assign s1_hit_d0 = (rs_data_addr[31:29] == csr_dmw0_vseg && csr_dmw0_plv[csr_crmd_plv]);
assign s1_hit_d1 = (rs_data_addr[31:29] == csr_dmw1_vseg && csr_dmw1_plv[csr_crmd_plv]);
assign s1_hit_d2 = (rs_data_addr[31:29] == csr_dmw2_vseg && csr_dmw2_plv[csr_crmd_plv]);
assign s1_hit_d3 = (rs_data_addr[31:29] == csr_dmw3_vseg && csr_dmw3_plv[csr_crmd_plv]);
assign s1_hit_d_mat = ({2{s1_hit_d0}} & csr_dmw0_mat) | ({2{s1_hit_d1}} & csr_dmw1_mat) | ({2{s1_hit_d2}} & csr_dmw2_mat) | ({2{s1_hit_d3}} & csr_dmw3_mat);
assign dmw_res1 = ({3{s1_hit_d0}} & csr_dmw0_pseg) | ({3{s1_hit_d1}} & csr_dmw1_pseg) | ({3{s1_hit_d2}} & csr_dmw2_pseg) | ({3{s1_hit_d3}} & csr_dmw3_pseg);
assign s1_hit_d = s1_hit_d0 | s1_hit_d1 | s1_hit_d2 | s1_hit_d3;

assign {csr_dmw3_plv      ,
        csr_dmw3_mat      ,
		csr_dmw3_pseg     , 
		csr_dmw3_vseg     ,
		csr_dmw2_plv      , 
        csr_dmw2_mat      ,
		csr_dmw2_pseg     , 
		csr_dmw2_vseg     , 
		csr_dmw1_plv      ,
        csr_dmw1_mat      ,
		csr_dmw1_pseg     ,
		csr_dmw1_vseg     ,
		csr_dmw0_plv      ,
        csr_dmw0_mat      ,
		csr_dmw0_pseg     ,
		csr_dmw0_vseg     , 
        csr_crmd_plv      ,
		csr_tlbrera_istlbr,
		csr_crmd_da       ,
		csr_crmd_pg       ,
		invtlb_asid       ,
		invtlb_vppn       ,
		invtlb_op         ,
		ws_is_invtlb      ,
		ws_is_tlbfill     ,
		csr_tlbidx_ne     ,
		csr_tlbidx_ps     ,
		csr_tlbelo1_v     ,
		csr_tlbelo1_d     ,
		csr_tlbelo1_plv   ,
		csr_tlbelo1_mat   ,
		csr_tlbelo1_g     ,
		csr_tlbelo1_ppn   ,
		csr_tlbelo0_v     ,
		csr_tlbelo0_d     ,
		csr_tlbelo0_plv   ,
		csr_tlbelo0_mat   ,
		csr_tlbelo0_g     ,
		csr_tlbelo0_ppn   ,
		ws_is_tlbwr       ,
		csr_tlbidx_index  ,   
		csr_asid_asid     ,
		ws_is_tlbsrch     ,
		csr_tlbehi_vppn} = ws_to_tlb_bus;

assign tlb_to_ws_bus = `TLB_TO_WS_BUS_WD'b0;
					   
//refetch
wire refetch;
wire ds_refetch, es_refetch, rs_refetch, ms_refetch, ws_refetch;
reg  ds_refetch_r;
assign refetch = ds_refetch || es_refetch || rs_refetch || ms_refetch || ws_refetch;
wire [31:0] fs_pc;
wire        fs_valid;
reg  [31:0] refetch_pc;

//address
wire is_direct_ad, is_map_ad;
wire [31:0] p_inst_addr, p_data_addr; //物理地址
assign is_direct_ad =  csr_crmd_da && !csr_crmd_pg;
assign is_map_ad    = !csr_crmd_da &&  csr_crmd_pg;
assign p_inst_addr = {is_map_ad ? {dmw_res0, ps_inst_addr[28:12]} : ps_inst_addr[31:12], ps_inst_addr[11:0]};
assign p_data_addr = {is_map_ad ? {dmw_res1, rs_data_addr[28:12]} : rs_data_addr[31:12], rs_data_addr[11:0]};

//tlb_ex
//tlb_to_ps_bus
assign tlb_to_ps_bus = {1'b0, //2
						1'b0, //1
						1'b0 //0
					   };
//tlb_to_rs_bus
assign tlb_to_rs_bus = {1'b0, //3
						1'b0, //2
						1'b0, //1
						1'b0 //0
					   };


always @(posedge clk) begin
	if (reset) begin
		refetch_pc <= 32'b0;
	end
	else if (ds_refetch && !ds_refetch_r) begin
		refetch_pc <= fs_valid ? fs_pc : ps_inst_addr;
	end
	
	ds_refetch_r <= ds_refetch;
end

//cache uncache
wire [1:0] datf, datm;

//icache
wire         icache_req      ;
wire         icache_op       ;
wire [  3:0] icache_wstrb    ;
wire [ 31:0] icache_wdata    ;
wire         icache_addr_ok  ;
wire         icache_data_ok  ;
wire [ 31:0] icache_rdata    ;
wire         icache_rd_req   ;
wire [  2:0] icache_rd_type  ;
wire [ 31:0] icache_rd_addr  ;
wire         icache_rd_rdy   ;
wire         icache_ret_valid;
wire         icache_ret_last ;
wire [ 31:0] icache_ret_data ;
wire         icache_wr_req   ;
wire [  2:0] icache_wr_type  ;
wire [ 31:0] icache_wr_addr  ;
wire [  3:0] icache_wr_wstrb ;
wire [127:0] icache_wr_data  ;
wire         icache_wr_rdy   ;
wire         icache_uncached ;

assign icache_op = |icache_wstrb;
assign icache_wr_rdy = 1'b1; //icache不会写内存，置为1没有副作用

assign icache_uncached = is_direct_ad && (datf != 2'b01) 
                      || (is_map_ad && (s0_hit_d_mat) != 2'b1);
assign dcache_uncached = is_direct_ad && (datm != 2'b01) 
                      || (is_map_ad && (s1_hit_d_mat) != 2'b1);

//dcache
wire         dcache_req      ;
wire         dcache_op       ;
wire [  3:0] dcache_wstrb    ;
wire [ 31:0] dcache_wdata    ;
wire         dcache_addr_ok  ;
wire         dcache_data_ok  ;
wire [ 31:0] dcache_rdata    ;
wire         dcache_rd_req   ;
wire [  2:0] dcache_rd_type  ;
wire [ 31:0] dcache_rd_addr  ;
wire         dcache_rd_rdy   ;
wire         dcache_ret_valid;
wire         dcache_ret_last ;
wire [ 31:0] dcache_ret_data ;
wire         dcache_wr_req   ;
wire [  2:0] dcache_wr_type  ;
wire [ 31:0] dcache_wr_addr  ;
wire [  3:0] dcache_wr_wstrb ;
wire [127:0] dcache_wr_data  ;
wire         dcache_wr_rdy   ;

assign dcache_op = |dcache_wstrb;

assign es_br_taken = br_bus[`BR_BUS_WD - 1];

//pre-IF stage
pre_if_stage pre_if_stage(
    .clk            (clk              ),
    .reset          (reset            ),
    //allowin
    .fs_allowin     (fs_allowin       ),
    //brbus
    .br_bus         (br_bus           ),
	//wrongPC_br
	.wrongPC_br     (wrongPC_br       ),
	.wrong_req_r    (wrong_req_r      ),
	.br_bus_r_valid (br_bus_r_valid   ),
    //outputs
    .ps_to_fs_valid (ps_to_fs_valid   ),
    .ps_to_fs_bus   (ps_to_fs_bus     ),
	//input
	.ws_to_ps_bus   (ws_to_ps_bus     ),
    //icache interface
    .inst_sram_en   (icache_req       ),
    .inst_sram_we   (icache_wstrb     ),
    .inst_sram_addr (ps_inst_addr     ),
    .inst_sram_wdata(icache_wdata     ),
	.addr_ok        (icache_addr_ok   ),
	.data_ok        (icache_data_ok   ),
	//tlb
	.tlb_to_ps_bus  (tlb_to_ps_bus    ),
	//refetch
	.refetch        (refetch          ),
	.refetch_pc     (refetch_pc       ),
	.ps_inst_req    (ps_inst_req      ),
    //cacop
    .fs_is_icacop   (fs_is_icacop     ),
    .ds_is_icacop   (ds_is_icacop     ),
    .es_is_icacop   (es_is_icacop     ),
    .rs_is_icacop   (rs_is_icacop     )
);

// IF stage
if_stage if_stage(
    .clk            (clk              ),
    .reset          (reset            ),
    //allowin
    .ds_allowin     (ds_allowin       ),
	.fs_allowin     (fs_allowin       ),
	//from ps
	.ps_to_fs_valid (ps_to_fs_valid   ),
	.ps_to_fs_bus   (ps_to_fs_bus     ),
	.wrongPC_br     (wrongPC_br       ),
	.wrong_req_r    (wrong_req_r      ),
	.br_bus_r_valid (br_bus_r_valid   ),
    //outputs
    .fs_to_ds_valid (fs_to_ds_valid   ),
    .fs_to_ds_bus   (fs_to_ds_bus     ),
	//from ws
	.ws_to_fs_bus   (ws_to_fs_bus     ),
    //from es
    .es_br_taken    (es_br_taken      ),
    //inst sram interface
    .inst_sram_rdata(icache_rdata     ),
	.data_ok        (icache_data_ok   ),
	//refetch
	.refetch        (refetch          ),
	.fs_pc          (fs_pc            ),
	.fs_valid       (fs_valid         ),
    //to ps
    .fs_is_icacop   (fs_is_icacop     )
);
// ID stage
id_stage id_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    //allowin
    .es_allowin     (es_allowin     ),
    .ds_allowin     (ds_allowin     ),
    //from fs
    .fs_to_ds_valid (fs_to_ds_valid ),
    .fs_to_ds_bus   (fs_to_ds_bus   ),
    //to es
    .ds_to_es_valid (ds_to_es_valid ),
    .ds_to_es_bus   (ds_to_es_bus   ),
    //to rf: for write back
    .ws_to_rf_bus   (ws_to_rf_bus   ),
	.es_to_ds_bus   (es_to_ds_bus   ),
    .rs_to_ds_bus   (rs_to_ds_bus   ),
	.ms_to_ds_bus   (ms_to_ds_bus   ),
	.ws_to_ds_bus   (ws_to_ds_bus   ),
	//refetch
	.ds_refetch     (ds_refetch     ),
    //cacop
    .ds_is_icacop   (ds_is_icacop   ),
    //from es
    .es_br_taken    (es_br_taken    )
    `ifdef DIFFTEST_EN
    ,
    .rf_to_diff     (regs           )
    `endif
);
// EXE stage
exe_stage exe_stage(
    .clk            (clk              ),
    .reset          (reset            ),
    //allowin
    .rs_allowin     (rs_allowin       ),
    .es_allowin     (es_allowin       ),
    //from ds
    .ds_to_es_valid (ds_to_es_valid   ),
    .ds_to_es_bus   (ds_to_es_bus     ),
    //to rs
    .es_to_rs_valid (es_to_rs_valid   ),
    .es_to_rs_bus   (es_to_rs_bus     ),
    //to ds
	.es_to_ds_bus   (es_to_ds_bus     ),
    //from ws
	.ws_to_es_bus   (ws_to_es_bus     ),
	//refetch
	.es_refetch     (es_refetch       ),
    //cacop
    .es_is_icacop   (es_is_icacop     ),
    .br_bus         (br_bus           )
);

//PRE-MEM stage
pre_mem_stage pre_mem_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    //allowin
    .ms_allowin     (ms_allowin     ),
    .rs_allowin     (rs_allowin     ),
    //from es
    .es_to_rs_valid (es_to_rs_valid ),
    .es_to_rs_bus   (es_to_rs_bus   ),
    //to ms
    .rs_to_ms_valid (rs_to_ms_valid ),
    .rs_to_ms_bus   (rs_to_ms_bus   ),
    //from ms
    .ms_to_rs_bus   (ms_to_rs_bus   ),
    //inst sram interface
    .data_sram_en   (dcache_req     ),
    .data_sram_we   (dcache_wstrb   ),
    .data_sram_addr (rs_data_addr   ),
    .data_sram_wdata(dcache_wdata   ),
    .addr_ok        (dcache_addr_ok ),
    //to ds
    .rs_to_ds_bus   (rs_to_ds_bus   ),
    .ws_to_rs_bus   (ws_to_rs_bus   ),
    //with tlb
    .tlb_to_rs_bus  (tlb_to_rs_bus  ),
    //refetch
    .rs_refetch     (rs_refetch     ),
    //cacop
    .rs_is_icacop   (rs_is_icacop   ),
    .rs_is_cacop    (rs_is_cacop    ),
    .cacop_code     (cacop_code     ),
    //to top
    .rs_data_req    (rs_data_req    )
);

// MEM stage
mem_stage mem_stage(
    .clk            (clk              ),
    .reset          (reset            ),
    //allowin
    .ws_allowin     (ws_allowin       ),
    .ms_allowin     (ms_allowin       ),
    //from rs
    .rs_to_ms_valid (rs_to_ms_valid   ),
    .rs_to_ms_bus   (rs_to_ms_bus     ),
    //to ws 
    .ms_to_ws_valid (ms_to_ws_valid   ),
    .ms_to_ws_bus   (ms_to_ws_bus     ),
    //from data-sram
    .data_sram_rdata(dcache_rdata     ),
	.data_ok        (dcache_data_ok   ),

	.ms_to_ds_bus   (ms_to_ds_bus     ),
    .ms_to_rs_bus   (ms_to_rs_bus     ),
	.ws_to_ms_bus   (ws_to_ms_bus     ),
	//refetch
	.ms_refetch     (ms_refetch       )
);
// WB stage
wb_stage wb_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    //allowin
    .ws_allowin     (ws_allowin     ),
    //from ms
    .ms_to_ws_valid (ms_to_ws_valid ),
    .ms_to_ws_bus   (ms_to_ws_bus   ),
    //to rf: for write back
    .ws_to_rf_bus   (ws_to_rf_bus   ),
    //trace debug interface
    .debug_wb_pc      (debug_wb_pc      ),
    .debug_wb_rf_we   (debug_wb_rf_we   ),
    .debug_wb_rf_wnum (debug_wb_rf_wnum ),
    .debug_wb_rf_wdata(debug_wb_rf_wdata),
	.ws_to_ds_bus     (ws_to_ds_bus     ),
	.ws_to_es_bus     (ws_to_es_bus     ),
    .ws_to_rs_bus     (ws_to_rs_bus     ),
	.ws_to_ms_bus     (ws_to_ms_bus     ),
	.ws_to_ps_bus     (ws_to_ps_bus     ),
	.ws_to_fs_bus     (ws_to_fs_bus     ),
	//with tlb
	.ws_to_tlb_bus    (ws_to_tlb_bus    ),
	.tlb_to_ws_bus    (tlb_to_ws_bus    ),
	//refetch
	.ws_refetch       (ws_refetch       ),
    //cache
    .datf             (datf             ),
    .datm             (datm             )

    //difftest
    `ifdef DIFFTEST_EN
    ,
    .debug_wb_inst   (debug_wb_inst     ),
    .ws_valid_diff   (ws_valid_diff     ),
    .cnt_inst_diff   (cnt_inst_diff     ),
    .timer_64_diff   (timer_64_diff     ),
    .inst_ld_en_diff (inst_ld_en_diff   ),
    .ld_paddr_diff   (ld_paddr_diff     ),
    .ld_vaddr_diff   (ld_vaddr_diff     ),
    .inst_st_en_diff (inst_st_en_diff   ),
    .st_paddr_diff   (st_paddr_diff     ),
    .st_vaddr_diff   (st_vaddr_diff     ),
    .st_data_diff    (st_data_diff      ),
    .csr_rstat_en_diff (csr_rstat_en_diff    ),
    .csr_data_diff   (csr_data_diff     )
    `endif
);

sram_axi_bridge bridge(
    .clk              (clk              ),
    .reset            (reset            ),

    .icache_rd_req    (icache_rd_req    ),
    .icache_rd_type   (icache_rd_type   ),
    .icache_rd_addr   (icache_rd_addr   ),
    .icache_rd_rdy    (icache_rd_rdy    ),
    .icache_ret_valid (icache_ret_valid ),
    .icache_ret_last  (icache_ret_last  ),
    .icache_ret_data  (icache_ret_data  ),

    .dcache_rd_req    (dcache_rd_req    ),
    .dcache_rd_type   (dcache_rd_type   ),
    .dcache_rd_addr   (dcache_rd_addr   ),
    .dcache_rd_rdy    (dcache_rd_rdy    ),
    .dcache_ret_valid (dcache_ret_valid ),
    .dcache_ret_last  (dcache_ret_last  ),
    .dcache_ret_data  (dcache_ret_data  ),
    .dcache_wr_req    (dcache_wr_req    ),
    .dcache_wr_type   (dcache_wr_type   ),
    .dcache_wr_addr   (dcache_wr_addr   ),
    .dcache_wr_wstrb  (dcache_wr_wstrb  ),
    .dcache_wr_data   (dcache_wr_data   ),
    .dcache_wr_rdy    (dcache_wr_rdy    ),
	
    .arid      (arid      ),
    .araddr    (araddr    ),
    .arlen     (arlen     ),
    .arsize    (arsize    ),
    .arburst   (arburst   ),
    .arlock    (arlock    ),
    .arcache   (arcache   ),
    .arprot    (arprot    ),
    .arvalid   (arvalid   ),
    .arready   (arready   ),
                
    .rid       (rid       ),
    .rdata     (rdata     ),
    .rresp     (rresp     ),
    .rlast     (rlast     ),
    .rvalid    (rvalid    ),
    .rready    (rready    ),
               
    .awid      (awid      ),
    .awaddr    (awaddr    ),
    .awlen     (awlen     ),
    .awsize    (awsize    ),
    .awburst   (awburst   ),
    .awlock    (awlock    ),
    .awcache   (awcache   ),
    .awprot    (awprot    ),
    .awvalid   (awvalid   ),
    .awready   (awready   ),
    
    .wid       (wid       ),
    .wdata     (wdata     ),
    .wstrb     (wstrb     ),
    .wlast     (wlast     ),
    .wvalid    (wvalid    ),
    .wready    (wready    ),
    
    .bid       (bid       ),
    .bresp     (bresp     ),
    .bvalid    (bvalid    ),
    .bready    (bready    )
);

//ps_inst_addr为虚地址，p_inst_addr为实地址
cache icache (
    .clk     (clk               ),
    .resetn  (~reset            ),
    .valid   (icache_req        ),
    .op      (icache_op         ),
    .index   (rs_is_icacop ? rs_data_addr[11:4] : ps_inst_addr[11:4]),
    .tag     (rs_is_icacop ? p_data_addr[31:12] : p_inst_addr[31:12]),
    .offset  (rs_is_icacop ? rs_data_addr[3:0] : ps_inst_addr[3:0] ),
    .wstrb   (icache_wstrb      ),
    .wdata   (icache_wdata      ),
    .addr_ok (icache_addr_ok    ),
    .data_ok (icache_data_ok    ),
    .rdata   (icache_rdata      ),
    .uncached(icache_uncached   ),
    //axi
    .rd_req   (icache_rd_req   ),
    .rd_type  (icache_rd_type  ),
    .rd_addr  (icache_rd_addr  ),
    .rd_rdy   (icache_rd_rdy   ),
    .ret_valid(icache_ret_valid),
    .ret_last (icache_ret_last ),
    .ret_data (icache_ret_data ),
    .wr_req   (icache_wr_req   ),
    .wr_type  (icache_wr_type  ),
    .wr_addr  (icache_wr_addr  ),
    .wr_wstrb (icache_wr_wstrb ),
    .wr_data  (icache_wr_data  ),
    .wr_rdy   (icache_wr_rdy   ),
    //cacop
    .cacop_req(rs_is_icacop    ),
    .cacop_op (cacop_op        )
);

cache dcache (
    .clk     (clk               ),
    .resetn  (~reset            ),
    .valid   (dcache_req        ),
    .op      (dcache_op         ),
    .index   (rs_data_addr[11:4]),
    .tag     (p_data_addr[31:12]),
    .offset  (rs_data_addr[3:0] ),
    .wstrb   (dcache_wstrb      ),
    .wdata   (dcache_wdata      ),
    .addr_ok (dcache_addr_ok    ),
    .data_ok (dcache_data_ok    ),
    .rdata   (dcache_rdata      ),
    .uncached(dcache_uncached   ),
    //axi
    .rd_req   (dcache_rd_req   ),
    .rd_type  (dcache_rd_type  ),
    .rd_addr  (dcache_rd_addr  ),
    .rd_rdy   (dcache_rd_rdy   ),
    .ret_valid(dcache_ret_valid),
    .ret_last (dcache_ret_last ),
    .ret_data (dcache_ret_data ),
    .wr_req   (dcache_wr_req   ),
    .wr_type  (dcache_wr_type  ),
    .wr_addr  (dcache_wr_addr  ),
    .wr_wstrb (dcache_wr_wstrb ),
    .wr_data  (dcache_wr_data  ),
    .wr_rdy   (dcache_wr_rdy   ),
    //cacop
    .cacop_req(rs_is_dcacop    ),
    .cacop_op (cacop_op        )
);
///////////////////////////////////////////////////
`ifdef DIFFTEST_EN
// difftest

always @(posedge aclk) begin
    if (reset) begin
        {cmt_valid, cmt_cnt_inst, cmt_timer_64, cmt_inst_ld_en, cmt_ld_paddr, cmt_ld_vaddr, cmt_inst_st_en, cmt_st_paddr, cmt_st_vaddr, cmt_st_data, cmt_csr_rstat_en, cmt_csr_data} <= 0;
        {cmt_wen, cmt_wdest, cmt_wdata, cmt_pc, cmt_inst} <= 0;
        {trap, trap_code, cycleCnt, instrCnt} <= 0;
    end else if (~trap) begin
        cmt_valid       <= inst_valid_diff          ;
        cmt_cnt_inst    <= cnt_inst_diff            ;
        cmt_timer_64    <= timer_64_diff            ;
        cmt_inst_ld_en  <= inst_ld_en_diff          ;
        cmt_ld_paddr    <= ld_paddr_diff            ;
        cmt_ld_vaddr    <= ld_vaddr_diff            ;
        cmt_inst_st_en  <= inst_st_en_diff          ;
        cmt_st_paddr    <= st_paddr_diff            ;
        cmt_st_vaddr    <= st_vaddr_diff            ;
        cmt_st_data     <= st_data_diff             ;
        cmt_csr_rstat_en<= csr_rstat_en_diff        ;
        cmt_csr_data    <= csr_data_diff            ;

        cmt_wen     <=  debug_wb_rf_we              ;
        cmt_wdest   <=  {3'd0, debug_wb_rf_wnum}    ;
        cmt_wdata   <=  debug_wb_rf_wdata           ;
        cmt_pc      <=  debug_wb_pc                 ;
        cmt_inst    <=  debug_wb_inst               ;

        cmt_excp_flush  <= 1'b0; //excp_flush               ;
        cmt_ertn        <= 1'b0; //ertn_flush               ;
        cmt_csr_ecode   <=6'b0; //                ;
        cmt_tlbfill_en  <= 1'b0; //tlbfill_en               ;
        cmt_rand_index  <= 5'b0; //rand_index               ;

        trap            <= 0                        ;
        trap_code       <= regs[10][7:0]            ;
        cycleCnt        <= cycleCnt + 1             ;
        instrCnt        <= instrCnt + inst_valid_diff;
    end
end

DifftestInstrCommit DifftestInstrCommit(
    .clock              (aclk           ),
    .coreid             (0              ),
    .index              (0              ),
    .valid              (cmt_valid      ),
    .pc                 (cmt_pc         ),
    .instr              (cmt_inst       ),
    .skip               (0              ),
    .is_TLBFILL         (/* cmt_tlbfill_en */1'b0 ),
    .TLBFILL_index      (/* cmt_rand_index */5'b0 ),
    .is_CNTinst         (cmt_cnt_inst   ),
    .timer_64_value     (cmt_timer_64   ),
    .wen                (cmt_wen        ),
    .wdest              (cmt_wdest      ),
    .wdata              (cmt_wdata      ),
    .csr_rstat          (cmt_csr_rstat_en),
    .csr_data           (cmt_csr_data   )
);

DifftestExcpEvent DifftestExcpEvent(
    .clock              (aclk           ),
    .coreid             (0              ),
    .excp_valid         (/* cmt_excp_flush */1'b0 ),
    .eret               (/* cmt_ertn */ 1'b0      ),
    .intrNo             (csr_estat_diff_0[12:2]),
    .cause              (cmt_csr_ecode  ),
    .exceptionPC        (cmt_pc         ),
    .exceptionInst      (cmt_inst       )
);

DifftestTrapEvent DifftestTrapEvent(
    .clock              (aclk           ),
    .coreid             (0              ),
    .valid              (trap           ),
    .code               (trap_code      ),
    .pc                 (cmt_pc         ),
    .cycleCnt           (cycleCnt       ),
    .instrCnt           (instrCnt       )
);

DifftestStoreEvent DifftestStoreEvent(
    .clock              (aclk           ),
    .coreid             (0              ),
    .index              (0              ),
    .valid              (cmt_inst_st_en ),
    .storePAddr         (cmt_st_paddr   ),
    .storeVAddr         (cmt_st_vaddr   ),
    .storeData          (cmt_st_data    )
);

DifftestLoadEvent DifftestLoadEvent(
    .clock              (aclk           ),
    .coreid             (0              ),
    .index              (0              ),
    .valid              (cmt_inst_ld_en ),
    .paddr              (cmt_ld_paddr   ),
    .vaddr              (cmt_ld_vaddr   )
);

DifftestCSRRegState DifftestCSRRegState(
    .clock              (aclk               ),
    .coreid             (0                  ),
    .crmd               (csr_crmd_diff_0    ),
    .prmd               (csr_prmd_diff_0    ),
    .euen               (0                  ),
    .ecfg               (csr_ectl_diff_0    ),
    .estat              (csr_estat_diff_0   ),
    .era                (csr_era_diff_0     ),
    .badv               (csr_badv_diff_0    ),
    .eentry             (csr_eentry_diff_0  ),
    .tlbidx             (csr_tlbidx_diff_0  ),
    .tlbehi             (csr_tlbehi_diff_0  ),
    .tlbelo0            (csr_tlbelo0_diff_0 ),
    .tlbelo1            (csr_tlbelo1_diff_0 ),
    .asid               (csr_asid_diff_0    ),
    .pgdl               (csr_pgdl_diff_0    ),
    .pgdh               (csr_pgdh_diff_0    ),
    .save0              (csr_save0_diff_0   ),
    .save1              (csr_save1_diff_0   ),
    .save2              (csr_save2_diff_0   ),
    .save3              (csr_save3_diff_0   ),
    .tid                (csr_tid_diff_0     ),
    .tcfg               (csr_tcfg_diff_0    ),
    .tval               (csr_tval_diff_0    ),
    .ticlr              (csr_ticlr_diff_0   ),
    .llbctl             (csr_llbctl_diff_0  ),
    .tlbrentry          (csr_tlbrentry_diff_0),
    .dmw0               (csr_dmw0_diff_0    ),
    .dmw1               (csr_dmw1_diff_0    )
);

DifftestGRegState DifftestGRegState(
    .clock              (aclk       ),
    .coreid             (0          ),
    .gpr_0              (0          ),
    .gpr_1              (regs[1]    ),
    .gpr_2              (regs[2]    ),
    .gpr_3              (regs[3]    ),
    .gpr_4              (regs[4]    ),
    .gpr_5              (regs[5]    ),
    .gpr_6              (regs[6]    ),
    .gpr_7              (regs[7]    ),
    .gpr_8              (regs[8]    ),
    .gpr_9              (regs[9]    ),
    .gpr_10             (regs[10]   ),
    .gpr_11             (regs[11]   ),
    .gpr_12             (regs[12]   ),
    .gpr_13             (regs[13]   ),
    .gpr_14             (regs[14]   ),
    .gpr_15             (regs[15]   ),
    .gpr_16             (regs[16]   ),
    .gpr_17             (regs[17]   ),
    .gpr_18             (regs[18]   ),
    .gpr_19             (regs[19]   ),
    .gpr_20             (regs[20]   ),
    .gpr_21             (regs[21]   ),
    .gpr_22             (regs[22]   ),
    .gpr_23             (regs[23]   ),
    .gpr_24             (regs[24]   ),
    .gpr_25             (regs[25]   ),
    .gpr_26             (regs[26]   ),
    .gpr_27             (regs[27]   ),
    .gpr_28             (regs[28]   ),
    .gpr_29             (regs[29]   ),
    .gpr_30             (regs[30]   ),
    .gpr_31             (regs[31]   )
);
`endif
endmodule