//LoongArch
`include "mycpu.h"

module mycpu_top(
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
wire fs_is_icacop, ds_is_icacop, es_is_icacop;
wire es_is_cacop, es_is_dcacop;
wire [4:0] cacop_code;
wire [1:0] cacop_op;
wire is_icacop = cacop_code[2:0] == 3'b0;
wire is_dcacop = cacop_code[2:0] == 3'b1;
assign cacop_op = cacop_code[4:3];
assign es_is_dcacop = es_is_cacop && !es_is_icacop;

wire clk, resetn;
assign clk = aclk;
assign resetn = aresetn;

//pre-IF_stage
wire [31:0] ps_inst_addr;
wire ps_inst_req;
//exe_stage
wire [31:0] es_data_addr;
wire es_data_req;

/* // inst sram interface
wire        inst_sram_req    ;
wire        inst_sram_wr     ;
wire [ 1:0] inst_sram_size   ;
wire [ 3:0] inst_sram_wstrb  ;
wire [31:0] inst_sram_addr   ;
wire [31:0] inst_sram_wdata  ;
wire        inst_sram_addr_ok;
wire        inst_sram_data_ok;
wire [31:0] inst_sram_rdata  ; */

/* // data sram interface
wire        data_sram_req    ;
wire        data_sram_wr     ;
wire [ 1:0] data_sram_size   ; 
wire [ 3:0] data_sram_wstrb  ;
wire [31:0] data_sram_addr   ;
wire [31:0] data_sram_wdata  ;
wire        data_sram_addr_ok; 
wire        data_sram_data_ok;
wire [31:0] data_sram_rdata  ; */

/* assign inst_sram_wr = |inst_sram_wstrb;
assign inst_sram_size = 2'b10; */
/* assign data_sram_wr = |data_sram_wstrb; */

reg         reset;
always @(posedge clk) reset <= ~resetn;

wire         fs_allowin;
wire         ds_allowin;
wire         es_allowin;
wire         ms_allowin;
wire         ws_allowin;
wire         ps_to_fs_valid;
wire         fs_to_ds_valid;
wire         ds_to_es_valid;
wire         es_to_ms_valid;
wire         ms_to_ws_valid;
wire [`PS_TO_FS_BUS_WD -1:0] ps_to_fs_bus;
wire [`FS_TO_DS_BUS_WD -1:0] fs_to_ds_bus;
wire [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus;
wire [`ES_TO_MS_BUS_WD -1:0] es_to_ms_bus;
wire [`MS_TO_WS_BUS_WD -1:0] ms_to_ws_bus;
wire [`WS_TO_RF_BUS_WD -1:0] ws_to_rf_bus;
wire [`BR_BUS_WD       -1:0] br_bus;

// forward
wire [`ES_TO_DS_BUS_WD - 1:0] es_to_ds_bus;
wire [`MS_TO_DS_BUS_WD - 1:0] ms_to_ds_bus;
wire [`WS_TO_DS_BUS_WD - 1:0] ws_to_ds_bus;

wire [`WS_TO_PS_BUS_WD - 1:0] ws_to_ps_bus;
wire [`WS_TO_FS_BUS_WD - 1:0] ws_to_fs_bus;
wire [`WS_TO_TLB_BUS_WD -1:0] ws_to_tlb_bus;
wire [`TLB_TO_WS_BUS_WD -1:0] tlb_to_ws_bus;
wire [`TLB_TO_PS_BUS_WD -1:0] tlb_to_ps_bus;
wire [`TLB_TO_ES_BUS_WD -1:0] tlb_to_es_bus;
wire [1:0] ws_to_es_bus;
wire [1:0] ws_to_ms_bus;
wire       ds_to_fs_bus;

wire       ms_to_es_bus;

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

//dmw
wire [3:0] csr_dmw3_plv, csr_dmw2_plv, csr_dmw1_plv, csr_dmw0_plv;
wire [1:0] csr_dmw3_mat, csr_dmw2_mat, csr_dmw1_mat, csr_dmw0_mat;
wire [1:0] s0_hit_d_mat, s1_hit_d_mat;
wire [2:0] csr_dmw3_pseg, csr_dmw3_vseg, csr_dmw2_pseg, csr_dmw2_vseg, csr_dmw1_pseg, csr_dmw1_vseg, csr_dmw0_pseg, csr_dmw0_vseg;
wire s0_hit_d0, s0_hit_d1, s0_hit_d2, s0_hit_d3, s1_hit_d0, s1_hit_d1, s1_hit_d2, s1_hit_d3, s0_hit, s1_hit;
wire [2:0] dmw_res0, dmw_res1;
assign s0_hit_d0 = (ps_inst_addr[31:29] == csr_dmw0_vseg && csr_dmw0_plv[csr_crmd_plv]);
assign s0_hit_d1 = (ps_inst_addr[31:29] == csr_dmw1_vseg && csr_dmw1_plv[csr_crmd_plv]);
assign s0_hit_d2 = (ps_inst_addr[31:29] == csr_dmw2_vseg && csr_dmw2_plv[csr_crmd_plv]);
assign s0_hit_d3 = (ps_inst_addr[31:29] == csr_dmw3_vseg && csr_dmw3_plv[csr_crmd_plv]);
assign s0_hit_d_mat = ({2{s0_hit_d0}} & csr_dmw0_mat) | ({2{s0_hit_d1}} & csr_dmw1_mat) | ({2{s0_hit_d2}} & csr_dmw2_mat) | ({2{s0_hit_d3}} & csr_dmw3_mat);
assign dmw_res0 = ({3{s0_hit_d0}} & csr_dmw0_pseg) | ({3{s0_hit_d1}} & csr_dmw1_pseg) | ({3{s0_hit_d2}} & csr_dmw2_pseg) | ({3{s0_hit_d3}} & csr_dmw3_pseg);
assign s0_hit = s0_hit_d0 | s0_hit_d1 | s0_hit_d2 | s0_hit_d3;
assign s1_hit_d0 = (es_data_addr[31:29] == csr_dmw0_vseg && csr_dmw0_plv[csr_crmd_plv]);
assign s1_hit_d1 = (es_data_addr[31:29] == csr_dmw1_vseg && csr_dmw1_plv[csr_crmd_plv]);
assign s1_hit_d2 = (es_data_addr[31:29] == csr_dmw2_vseg && csr_dmw2_plv[csr_crmd_plv]);
assign s1_hit_d3 = (es_data_addr[31:29] == csr_dmw3_vseg && csr_dmw3_plv[csr_crmd_plv]);
assign s1_hit_d_mat = ({2{s1_hit_d0}} & csr_dmw0_mat) | ({2{s1_hit_d1}} & csr_dmw1_mat) | ({2{s1_hit_d2}} & csr_dmw2_mat) | ({2{s1_hit_d3}} & csr_dmw3_mat);
assign dmw_res1 = ({3{s1_hit_d0}} & csr_dmw0_pseg) | ({3{s1_hit_d1}} & csr_dmw1_pseg) | ({3{s1_hit_d2}} & csr_dmw2_pseg) | ({3{s1_hit_d3}} & csr_dmw3_pseg);
assign s1_hit = s1_hit_d0 | s1_hit_d1 | s1_hit_d2 | s1_hit_d3;

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
// search port 0 (for fetch)
assign s0_vppn = ps_inst_addr[31:13];
assign s0_va_bit12 = ps_inst_addr[12];
assign s0_asid = csr_asid_asid;
// search port 1 (for load/store)
assign s1_vppn = ws_is_tlbsrch ? csr_tlbehi_vppn :
				 ws_is_invtlb  ? invtlb_vppn     :
				 es_data_addr[31:13];
assign s1_va_bit12 = es_data_addr[12];
assign s1_asid = ws_is_tlbsrch ? csr_asid_asid : 
				 ws_is_invtlb  ? invtlb_asid   :
				 csr_asid_asid;
// invtlb opcode
assign invtlb_valid = ws_is_invtlb;
// write port
assign we = ws_is_tlbwr | ws_is_tlbfill;
assign w_index = csr_tlbidx_index;
assign w_e = csr_tlbrera_istlbr ? 1'b1 : ~csr_tlbidx_ne;
assign w_vppn = csr_tlbehi_vppn;
assign w_ps = csr_tlbidx_ps;
assign w_asid = csr_asid_asid;
assign w_g = csr_tlbelo0_g & csr_tlbelo1_g;
assign w_ppn0 = csr_tlbelo0_ppn;
assign w_plv0 = csr_tlbelo0_plv;
assign w_mat0 = csr_tlbelo0_mat;
assign w_d0 = csr_tlbelo0_d;
assign w_v0 = csr_tlbelo0_v;
assign w_ppn1 = csr_tlbelo1_ppn;
assign w_plv1 = csr_tlbelo1_plv;
assign w_mat1 = csr_tlbelo1_mat;
assign w_d1 = csr_tlbelo1_d;
assign w_v1 = csr_tlbelo1_v;
// read port
assign r_index = csr_tlbidx_index;

assign tlb_to_ws_bus = {r_asid, //93:84
						r_ps, //83:78
						r_g, //77
						r_ppn0, //76:57
						r_plv0, //56:55
						r_mat0, //54:53
						r_d0, //52
						r_v0, //51
						r_ppn1, //50:31
						r_plv1, //30:29
						r_mat1, //28:27
						r_d1, //26
						r_v1, //25
						r_e, //24
						s1_index, //23:20
						s1_found, //19
						r_vppn // 18:0
					   };
					   
//refetch
wire refetch;
wire ds_refetch, es_refetch, ms_refetch, ws_refetch;
reg  ds_refetch_r;
assign refetch = ds_refetch || es_refetch || ms_refetch || ws_refetch;
wire [31:0] fs_pc;
wire        fs_valid;
reg  [31:0] refetch_pc;

//address
wire is_direct_ad, is_map_ad;
wire [31:0] p_inst_addr, p_data_addr; //物理地址
assign is_direct_ad =  csr_crmd_da && !csr_crmd_pg;
assign is_map_ad    = !csr_crmd_da &&  csr_crmd_pg;
assign p_inst_addr = {is_map_ad ? (s0_hit ? {dmw_res0, ps_inst_addr[28:12]} : {s0_ppn[19:9], ((s0_ps == 6'h15) ? ps_inst_addr[20:12] : s0_ppn[8:0])}) : ps_inst_addr[31:12], ps_inst_addr[11:0]};
assign p_data_addr = {is_map_ad ? (s1_hit ? {dmw_res1, es_data_addr[28:12]} : {s1_ppn[19:9], ((s1_ps == 6'h15) ? es_data_addr[20:12] : s1_ppn[8:0])}) : es_data_addr[31:12], es_data_addr[11:0]};

//tlb_ex
wire s0_refill_ex, s0_page_inv_ex, s0_ppi_ex;
wire s1_refill_ex, s1_page_inv_ex, s1_ppi_ex, s1_pme_ex;
assign s0_refill_ex = ps_inst_req && !s0_found && is_map_ad && !s0_hit;
assign s0_page_inv_ex = ps_inst_req && is_map_ad && s0_found && !s0_v && !s0_hit;
assign s0_ppi_ex = ps_inst_req && is_map_ad && (csr_crmd_plv > s0_plv) && !s0_refill_ex && !s0_page_inv_ex && !s0_hit;
wire s1_req;
assign s1_req = (es_data_req || es_is_cacop && cacop_code[4:3] == 2'b10);
assign s1_refill_ex = s1_req && !s1_found && is_map_ad && !s1_hit;
assign s1_page_inv_ex = s1_req && is_map_ad && s1_found && !s1_v && !s1_hit;
assign s1_ppi_ex = s1_req && is_map_ad && (csr_crmd_plv > s1_plv) && !s1_refill_ex && !s1_page_inv_ex && !s1_hit;
assign s1_pme_ex = s1_req && is_map_ad && s1_found && s1_v && !s1_ppi_ex && !s1_d && !s1_hit;

//tlb_to_ps_bus
assign tlb_to_ps_bus = {s0_ppi_ex, //2
						s0_page_inv_ex, //1
						s0_refill_ex //0
					   };
					   
//tlb_to_es_bus
assign tlb_to_es_bus = {s1_pme_ex, //3
						s1_ppi_ex, //2
						s1_page_inv_ex, //1
						s1_refill_ex //0
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
                      || (is_map_ad && (s0_hit ? s0_hit_d_mat : s0_mat) != 2'b1);
assign dcache_uncached = is_direct_ad && (datm != 2'b01) 
                      || (is_map_ad && (s1_hit ? s1_hit_d_mat : s1_mat) != 2'b1)
                      || p_data_addr[31:16] == 16'hbfaf; //这一行应该可以注释掉了

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
    .inst_sram_en   (icache_req       ),//inst_sram_req    ),
    .inst_sram_we   (icache_wstrb     ),//inst_sram_wstrb  ),
    .inst_sram_addr (ps_inst_addr     ),
    .inst_sram_wdata(icache_wdata     ),//inst_sram_wdata  ),
	.addr_ok        (icache_addr_ok   ),//inst_sram_addr_ok),
	.data_ok        (icache_data_ok   ),//inst_sram_data_ok),
	//tlb
	.tlb_to_ps_bus  (tlb_to_ps_bus    ),
	//refetch
	.refetch        (refetch          ),
	.refetch_pc     (refetch_pc       ),
	.ps_inst_req    (ps_inst_req      ),
    //cacop
    .fs_is_icacop   (fs_is_icacop     ),
    .ds_is_icacop   (ds_is_icacop     ),
    .es_is_icacop   (es_is_icacop     )
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
	//from ds
	.ds_to_fs_bus   (ds_to_fs_bus     ),
	//from ws
	.ws_to_fs_bus   (ws_to_fs_bus     ),
    //inst sram interface
    .inst_sram_rdata(icache_rdata     ), //inst_sram_rdata  ),
	.data_ok        (icache_data_ok   ), //inst_sram_data_ok),
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
    //to ps
    .br_bus         (br_bus         ),
	//to fs
	.ds_to_fs_bus   (ds_to_fs_bus   ),
    //to rf: for write back
    .ws_to_rf_bus   (ws_to_rf_bus   ),
	.es_to_ds_bus   (es_to_ds_bus   ),
	.ms_to_ds_bus   (ms_to_ds_bus   ),
	.ws_to_ds_bus   (ws_to_ds_bus   ),
	//refetch
	.ds_refetch     (ds_refetch     ),
    //cacop
    .ds_is_icacop   (ds_is_icacop   )
);
// EXE stage
exe_stage exe_stage(
    .clk            (clk              ),
    .reset          (reset            ),
    //allowin
    .ms_allowin     (ms_allowin       ),
    .es_allowin     (es_allowin       ),
    //from ds
    .ds_to_es_valid (ds_to_es_valid   ),
    .ds_to_es_bus   (ds_to_es_bus     ),
    //to ms
    .es_to_ms_valid (es_to_ms_valid   ),
    .es_to_ms_bus   (es_to_ms_bus     ),
    // data sram interface
    .data_sram_en   (dcache_req       ), //data_sram_req    ),
    .data_sram_we   (dcache_wstrb     ), //data_sram_wstrb  ),
/* 	.data_sram_size (data_sram_size   ), */
    .data_sram_addr (es_data_addr     ),
    .data_sram_wdata(dcache_wdata     ), //data_sram_wdata  ),
	.es_to_ds_bus   (es_to_ds_bus     ),
	.ws_to_es_bus   (ws_to_es_bus     ),
	.ms_to_es_bus   (ms_to_es_bus     ),
	.addr_ok        (dcache_addr_ok   ), //data_sram_addr_ok),
	.data_ok        (dcache_data_ok   ), //data_sram_data_ok),
	//with tlb
	.tlb_to_es_bus  (tlb_to_es_bus    ),
	//refetch
	.es_refetch     (es_refetch       ),
	.es_data_req    (es_data_req      ),
    //cacop
    .es_is_icacop   (es_is_icacop     ),
    .es_is_cacop    (es_is_cacop      ),
    .cacop_code     (cacop_code       )
);
// MEM stage
mem_stage mem_stage(
    .clk            (clk              ),
    .reset          (reset            ),
    //allowin
    .ws_allowin     (ws_allowin       ),
    .ms_allowin     (ms_allowin       ),
    //from es
    .es_to_ms_valid (es_to_ms_valid   ),
    .es_to_ms_bus   (es_to_ms_bus     ),
    //to ws 
    .ms_to_ws_valid (ms_to_ws_valid   ),
    .ms_to_ws_bus   (ms_to_ws_bus     ),
    //from data-sram
    .data_sram_rdata(dcache_rdata     ), //data_sram_rdata  ),
	.data_ok        (dcache_data_ok   ), //data_sram_data_ok),
	.ms_to_ds_bus   (ms_to_ds_bus     ),
	.ms_to_es_bus   (ms_to_es_bus     ),
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
/*     .icache_wr_req    (icache_wr_req    ),
    .icache_wr_type   (icache_wr_type   ),
    .icache_wr_addr   (icache_wr_addr   ),
    .icache_wr_wstrb  (icache_wr_wstrb  ),
    .icache_wr_data   (icache_wr_data   ),
    .icache_wr_rdy    (icache_wr_rdy    ), */

/*   //inst_sram_req    ),
    .inst_sram_wr     (inst_sram_wr     ),
    .inst_sram_size   (inst_sram_size   ),
    .inst_sram_wstrb  (inst_sram_wstrb  ),
    .inst_sram_addr   (inst_sram_addr   ),
    .inst_sram_wdata  (inst_sram_wdata  ),
    .inst_sram_addr_ok(inst_sram_addr_ok),
    .inst_sram_data_ok(inst_sram_data_ok),
    .inst_sram_rdata  (inst_sram_rdata  ), */

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
    
/*     .data_sram_req    (data_sram_req    ),
    .data_sram_wr     (data_sram_wr     ),
    .data_sram_size   (data_sram_size   ),
    .data_sram_wstrb  (data_sram_wstrb  ),
    .data_sram_addr   (data_sram_addr   ),
    .data_sram_wdata  (data_sram_wdata  ),
    .data_sram_addr_ok(data_sram_addr_ok),
    .data_sram_data_ok(data_sram_data_ok),
    .data_sram_rdata  (data_sram_rdata  ), */
	
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

tlb tlb (
    .clk          (clk         ),

    // search port 0 (for fetch)
    .s0_vppn      (s0_vppn     ),
    .s0_va_bit12  (s0_va_bit12 ),
    .s0_asid      (s0_asid     ),
    .s0_found     (s0_found    ),
    .s0_index     (s0_index    ),
    .s0_ppn       (s0_ppn      ),
    .s0_ps        (s0_ps       ),
    .s0_plv       (s0_plv      ),
    .s0_mat       (s0_mat      ),
    .s0_d         (s0_d        ),
    .s0_v         (s0_v        ),

    // search port 1 (for load/store)
    .s1_vppn      (s1_vppn     ),
    .s1_va_bit12  (s1_va_bit12 ),
    .s1_asid      (s1_asid     ),
    .s1_found     (s1_found    ),
    .s1_index     (s1_index    ),
    .s1_ppn       (s1_ppn      ),
    .s1_ps        (s1_ps       ),
    .s1_plv       (s1_plv      ),
    .s1_mat       (s1_mat      ),
    .s1_d         (s1_d        ),
    .s1_v         (s1_v        ),

    // invtlb opcode
    .invtlb_valid (invtlb_valid),
    .invtlb_op    (invtlb_op   ),

    // write port
    .we           (we          ),
    .w_index      (w_index     ),
    .w_e          (w_e         ),
    .w_vppn       (w_vppn      ),
    .w_ps         (w_ps        ),
    .w_asid       (w_asid      ),
    .w_g          (w_g         ),
    .w_ppn0       (w_ppn0      ),
    .w_plv0       (w_plv0      ),
    .w_mat0       (w_mat0      ),
    .w_d0         (w_d0        ),
    .w_v0         (w_v0        ),
    .w_ppn1       (w_ppn1      ),
    .w_plv1       (w_plv1      ),
    .w_mat1       (w_mat1      ),
    .w_d1         (w_d1        ),
    .w_v1         (w_v1        ),

    // read port
    .r_index      (r_index     ),
    .r_e          (r_e         ),
    .r_vppn       (r_vppn      ),
    .r_ps         (r_ps        ),
    .r_asid       (r_asid      ),
    .r_g          (r_g         ),
    .r_ppn0       (r_ppn0      ),
    .r_plv0       (r_plv0      ),
    .r_mat0       (r_mat0      ),
    .r_d0         (r_d0        ),
    .r_v0         (r_v0        ),
    .r_ppn1       (r_ppn1      ),
    .r_plv1       (r_plv1      ),
    .r_mat1       (r_mat1      ),
    .r_d1         (r_d1        ),
    .r_v1         (r_v1        )
);

//ps_inst_addr为虚地址，p_inst_addr为实地址
cache icache (
    .clk     (clk               ),
    .resetn  (~reset            ),
    .valid   (icache_req        ),
    .op      (icache_op         ),
    .index   (es_is_icacop ? es_data_addr[11:4] : ps_inst_addr[11:4]),
    .tag     (es_is_icacop ? p_data_addr[31:12] : p_inst_addr[31:12]),
    .offset  (es_is_icacop ? es_data_addr[3:0] : ps_inst_addr[3:0] ),
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
    .cacop_req(es_is_icacop    ),
    .cacop_op (cacop_op        )
);

cache dcache (
    .clk     (clk               ),
    .resetn  (~reset            ),
    .valid   (dcache_req        ),
    .op      (dcache_op         ),
    .index   (es_data_addr[11:4]),
    .tag     (p_data_addr[31:12]),
    .offset  (es_data_addr[3:0] ),
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
    .cacop_req(es_is_dcacop    ),
    .cacop_op (cacop_op        )
);
endmodule