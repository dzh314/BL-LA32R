`include "mycpu.h"

module exe_stage(
    input                          clk            ,
    input                          reset          ,
    //allowin
	input                          rs_allowin     ,
    output                         es_allowin     ,
    //from ds
    input                          ds_to_es_valid ,
    input  [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus   ,
	//to rs
	output                         es_to_rs_valid ,
	output [`ES_TO_RS_BUS_WD -1:0] es_to_rs_bus   ,

	output [`ES_TO_DS_BUS_WD -1:0] es_to_ds_bus   ,
	input  [                  1:0] ws_to_es_bus   ,
	//refetch
	output                         es_refetch     ,
	//cacop
	output                         es_is_icacop   ,
	//to ps
	output [`BR_BUS_WD       -1:0] br_bus         
);
wire [31:0] es_inst;

reg es_valid;
wire es_ready_go;

wire es_is_icacop_h;
assign es_is_icacop = es_is_icacop_h && es_valid;

wire ws_ertn, ws_ex;
assign {ws_ertn, ws_ex} = ws_to_es_bus;

wire es_refetch_help;

wire br_taken;
wire [31:0] br_target;

reg  [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus_r;

wire		es_mem_we      ;
wire		es_is_ld_w     ;
wire		es_is_ld_b     ;
wire		es_is_ld_h     ;
wire		es_is_ld_bu    ;
wire		es_is_ld_hu    ;
wire        es_is_st_w     ;
wire		es_is_st_b     ;
wire		es_is_st_h     ;
wire		es_is_csrrd    ;
wire		es_is_csrxchg  ;
wire		es_is_ertn     ;
wire		es_is_csrwr    ;
wire		es_is_rdtimeh_w;
wire		es_is_rdtimel_w;
wire		es_is_tlbsrch  ;
wire		es_is_tlbrd    ;
wire		es_is_tlbwr    ;
wire		es_is_tlbfill  ;
wire		es_is_invtlb   ;
wire [13:0] csr            ;
wire [18:0] es_alu_op      ;
wire		es_res_from_mem;
wire		es_gr_we       ;
wire		src2_is_imm    ;
wire		src1_is_pc     ;
wire [ 4:0] es_dest        ;
wire [31:0] es_rj_value    ;
wire [31:0] es_rkd_value   ;
wire [31:0] es_pc          ;
wire [31:0] es_imm         ;
wire [31:0] es_final_result;
wire        es_is_ld       ;

wire res_from_csr;
assign res_from_csr = es_is_csrrd | es_is_csrwr | es_is_csrxchg;

wire res_from_rdtime;
assign res_from_rdtime = es_is_rdtimeh_w | es_is_rdtimel_w;

wire is_load_or_store;
assign is_load_or_store = es_mem_we | es_is_ld_w | es_is_ld_b | es_is_ld_h | es_is_ld_bu | es_is_ld_hu;

//div_w
wire is_mul_w  ;
wire is_mulh_w ;
wire is_mulh_wu;
wire is_div_w  ;
wire is_div_wu ;
wire is_mod_w  ;
wire is_mod_wu ;
wire is_div    ;
wire is_divu   ;
assign is_div  = is_div_w  || is_mod_w ;
assign is_divu = is_div_wu || is_mod_wu;
reg [63:0] unsigned_prod_r, signed_prod_r;
reg first_cycle;
wire mul_stall;
assign mul_stall = (is_mul_w | is_mulh_w | is_mulh_wu) & first_cycle;
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
reg div_hs, divu_hs;
always @(posedge clk) begin
	if(reset) begin
		div_hs <= 1'b1;
	end
	else if(is_div && div_divisor_tvalid) begin
		div_hs <= 1'b0;
	end
	else if(div_w_ready || !is_div)
		div_hs <= 1'b1;
	else div_hs <= div_hs;
end
always @(posedge clk) begin
	if(reset) begin
		divu_hs <= 1'b1;
	end
	else if(is_divu && divu_divisor_tvalid) begin
		divu_hs <= 1'b0;
	end
	else if(div_wu_ready || !is_divu)
		divu_hs <= 1'b1;
	else divu_hs <= divu_hs;
end
assign div_divisor_tvalid   = is_div  & div_divisor_tready   & div_hs  && es_valid;
assign div_dividend_tvalid  = is_div  & div_dividend_tready  & div_hs  && es_valid;
assign divu_divisor_tvalid  = is_divu & divu_divisor_tready  & divu_hs && es_valid;
assign divu_dividend_tvalid = is_divu & divu_dividend_tready & divu_hs && es_valid;
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
	   
assign {es_inst        ,
		br_taken       ,
		br_target      ,
		es_is_cacop_h  ,
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

assign es_is_st_w = es_mem_we && !es_is_st_b && !es_is_st_h;

assign br_bus = {br_taken && es_valid && !(ws_ex || ws_ertn), br_target};

wire [31:0] es_alu_src1   ;
wire [31:0] es_alu_src2   ;
wire [31:0] es_alu_result ;

wire special_res;
assign special_res = is_div_w | is_div_wu | is_mul_w | is_mulh_w | is_mulh_wu | is_mod_w | is_mod_wu;

assign es_final_result = special_res ?
						({32{is_div_w}}   & div_w_res[63:32]       |
						 {32{is_div_wu}}  & div_wu_res[63:32]      | 
						 {32{is_mul_w}}   & signed_prod_r[31:0]    |  
						 {32{is_mulh_w}}  & signed_prod_r[63:32]   | 
						 {32{is_mulh_wu}} & unsigned_prod_r[63:32] |  
						 {32{is_mod_w}}   & div_w_res[31:0]        | 
						 {32{is_mod_wu}}  & div_wu_res[31:0]     ) : 
						 es_alu_result;
						 
assign es_refetch = es_refetch_help && es_valid;

assign es_to_rs_bus = {es_inst            , //237:206
					   is_load_or_store   , //205
					   es_is_cacop_h      , //204
					   es_is_icacop_h     , //203
					   es_is_ld           , //202
					   es_mem_we          , //201
					   es_is_st_b         , //200
					   es_is_st_h         , //199
					   es_is_st_w         , //198
					   es_rkd_value       , //197:166
					   s0_ex              , //165
					   s0_refill_ex       , //164
					   es_refetch         , //163
					   invtlb_vppn        , //162:144
					   invtlb_op          , //143:139
					   es_is_tlbsrch      , //138
					   es_is_tlbrd        , //137
					   es_is_tlbwr        , //136
					   es_is_tlbfill      , //135
					   es_is_invtlb       , //134
					   es_is_rdtimeh_w    , //133
					   es_is_rdtimel_w    , //132
					   es_ecode           , //131:126
					   es_ex              , //125
					   es_is_ertn         , //124
					   es_rj_value        , //123:92
					   es_is_csrxchg      , //91
					   es_is_csrwr        , //90
					   es_is_csrrd        , //89
					   csr                , //88:75
					   es_is_ld_hu        , //74
					   es_is_ld_bu        , //73
					   es_is_ld_h         , //72
					   es_is_ld_b         , //71
					   es_res_from_mem    , //70 其实是es_is_ld_w
					   es_gr_we           , //69
					   es_dest            , //68:64
					   es_final_result    , //63:32
					   es_pc                //31:0
};

// exception
assign es_ex = (ds_ex) && !ws_ertn && es_valid;
assign es_ecode = ds_ecode;

assign es_ready_go = (!(is_div || is_divu || mul_stall) || is_div && div_w_ready || is_divu && div_wu_ready) || es_ex;
assign es_allowin = !es_valid || es_ready_go && rs_allowin;
assign es_to_rs_valid =  es_valid && es_ready_go;

always @(posedge clk) begin
    if (reset) begin
        es_valid <= 1'b0;
		ds_to_es_bus_r <= `DS_TO_ES_BUS_WD'b0;
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
	end

	if (reset) begin
		first_cycle <= 1'b1;
	end
	else if (ds_to_es_valid && es_allowin) begin
		first_cycle <= 1'b1;
	end
	else if (first_cycle) begin
		first_cycle <= 1'b0;
	end

	if (is_mul_w | is_mulh_w) begin
		signed_prod_r <= signed_prod;
	end
	
	if (is_mulh_wu) begin
		unsigned_prod_r <= unsigned_prod;
	end
end
									  
assign es_alu_src1 = (es_is_csrwr | es_is_csrxchg) ? 32'b0 : src1_is_pc  ? es_pc[31:0] : es_rj_value;
/* assign es_alu_src1 = ({32{src1_is_pc}} & es_pc[31:0] | {32{~src1_is_pc}} & es_rj_value) & {32{~(es_is_csrwr | es_is_csrxchg)}}; */
assign es_alu_src2 = src2_is_imm ? es_imm : es_rkd_value;

alu u_alu(
    .alu_op     (es_alu_op[11:0]),
    .alu_src1   (es_alu_src1    ),
    .alu_src2   (es_alu_src2    ),
    .alu_result (es_alu_result  )
    );
						 
wire es_tlb_stall;
assign es_tlb_stall = (es_is_tlbsrch || es_is_invtlb || es_is_tlbfill) && es_valid;

assign es_is_ld = es_is_ld_hu || es_is_ld_bu || es_is_ld_h || es_is_ld_b || es_is_ld_w;
wire es_special_res; //result from memory, csr or mul/div
assign es_special_res = es_is_ld | res_from_csr | special_res;
//如果这个出现在关键路径上，就用指令本身的特点，在ID解码出这属于乘法类和除法类
assign es_to_ds_bus = {res_from_rdtime,
					   es_tlb_stall, 
					   es_special_res,
					   es_valid, 
					   es_gr_we, 
					   es_dest, 
					   es_alu_result
};
					   
endmodule

`ifdef DIFFTEST_EN
module mydivu(
    input           aclk,
    input   [31:0]  s_axis_dividend_tdata,   //被除数
    input   [31:0]  s_axis_divisor_tdata,    //除数
	input           s_axis_divisor_tvalid,
	output          s_axis_divisor_tready,
	input           s_axis_dividend_tvalid,
	output          s_axis_dividend_tready,
	output [63:0]   m_axis_dout_tdata     ,
	output          m_axis_dout_tvalid
);
wire  [31:0]  q;          //商
wire  [31:0]  r;          //余数
wire start;
reg busy;

assign start = s_axis_divisor_tvalid & s_axis_dividend_tvalid;
assign s_axis_dividend_tready = ~busy;
assign s_axis_divisor_tready = ~busy;
assign m_axis_dout_tdata = {q, r};
assign m_axis_dout_tvalid = !s_axis_divisor_tvalid && !s_axis_dividend_tvalid && !busy;

reg     [4:0]   count;
reg     [31:0]  reg_q;
reg     [31:0]  reg_r;
reg     [31:0]  reg_b;
reg             r_sign;
wire    [32:0]  sub_add = r_sign? ({reg_r,q[31]} + {1'b0,reg_b}):({reg_r,q[31]} - {1'b0,reg_b}); //加、减法器
assign r = r_sign? reg_r + reg_b : reg_r;
assign q = reg_q;
    
always @(posedge aclk)
begin
        if (start&&(!busy)) 
        begin //初始化
            reg_r   <= 32'b0;
            r_sign  <= 0;
            reg_q   <= s_axis_dividend_tdata;
            reg_b   <= s_axis_divisor_tdata;
            count   <= 5'b0;
            busy    <= 1'b1;
        end 
        else if (busy) 
            begin //循环操作
            reg_r   <= sub_add[31:0];   //部分余数
            r_sign  <= sub_add[32];
            reg_q   <= {reg_q[30:0],~sub_add[32]};
            count   <= count + 5'b1;    //计数器加一
            if (count == 5'b11111)  busy <= 0;
        end
end
endmodule

module mydiv(
    input           aclk,
    input   [31:0]  s_axis_dividend_tdata,   //被除数
    input   [31:0]  s_axis_divisor_tdata,    //除数
	input           s_axis_divisor_tvalid,
	input           s_axis_dividend_tvalid,
	output          s_axis_divisor_tready,
	output          s_axis_dividend_tready,

    output  [63:0]  m_axis_dout_tdata,
	output          m_axis_dout_tvalid
);
wire start;
reg busy;
assign start = s_axis_divisor_tvalid & s_axis_dividend_tvalid;
assign s_axis_divisor_tready = ~busy;
assign s_axis_dividend_tready = ~busy;
wire [31:0] q;//商
wire [31:0] r;//余数
assign m_axis_dout_tdata = {q, r};
assign m_axis_dout_tvalid = !s_axis_divisor_tvalid && !s_axis_dividend_tvalid && !busy;

    reg     [4:0]   count;
    reg     [31:0]  reg_q;
    reg     [31:0]  reg_r;
    reg     [31:0]  reg_b;
    reg             q_sign;
    reg             r_sign;
    reg             a_sign;
    wire    [32:0]  sub_add = r_sign? {reg_r,reg_q[31]}+{1'b0,reg_b}:{reg_r,reg_q[31]}-{1'b0,reg_b};
    assign r = a_sign? (-(r_sign? reg_r+reg_b:reg_r)):(r_sign? reg_r+reg_b:reg_r);
    assign q = q_sign? -reg_q:reg_q;
    
    always@(posedge aclk)
    begin
		if(start&&(!busy))
        begin 
            count<=0;reg_q<=s_axis_dividend_tdata[31]?-s_axis_dividend_tdata:s_axis_dividend_tdata;
            reg_r<=0;
            reg_b<=s_axis_divisor_tdata[31]?-s_axis_divisor_tdata:s_axis_divisor_tdata;
            r_sign<=0;busy<=1'b1;
            q_sign<=s_axis_dividend_tdata[31]^s_axis_divisor_tdata[31];
            a_sign<=s_axis_dividend_tdata[31];
        end
        else if(busy)
        begin
            reg_r<=sub_add[31:0];
            r_sign<=sub_add[32];
            reg_q<={reg_q[30:0],~sub_add[32]};
            count<=count+1;
            if(count==31)   busy<=0;
        end
    end
endmodule

`endif