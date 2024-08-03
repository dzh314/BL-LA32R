`include "mycpu.h"

module if_stage(
    input                          clk            ,
    input                          reset          ,
    //allwoin
    input                          ds_allowin     ,
	output                         fs_allowin     ,
	//from ps
	input                          ps_to_fs_valid ,
	input  [`PS_TO_FS_BUS_WD -1:0] ps_to_fs_bus   ,
	input                          wrongPC_br     ,
	input                          wrong_req_r    ,
	input                          br_bus_r_valid ,
    //to ds
    output                         fs_to_ds_valid ,
    output [`FS_TO_DS_BUS_WD -1:0] fs_to_ds_bus   ,
	//from ws
	input  [`WS_TO_FS_BUS_WD -1:0] ws_to_fs_bus   ,
	//from es
	input                          es_br_taken    ,
    // inst sram interface
    input  [                 31:0] inst_sram_rdata,
	input                          data_ok        ,
	//refetch
	input                          refetch        ,
	output [                 31:0] fs_pc          ,
	output reg                     fs_valid       ,
	//to ps
	output                         fs_is_icacop
);

wire        fs_ready_go;
reg         wrong_data_ok; //已经发送了请求，还未收到data_ok，发生ws_ex
wire        next_data_ok_is_wrong;
assign      next_data_ok_is_wrong = wrong_data_ok || wrong_req_r;

wire [ 5:0] op_31_26;
wire [ 3:0] op_25_22;
wire [ 1:0] op_21_20;
wire [ 4:0] op_19_15;
wire [ 4:0] op_14_10; //rk;
wire [ 4:0] op_9_5;   //rj;
wire [ 4:0] op_4_0;   //rd;

wire [63:0] op_31_26_d;
wire [15:0] op_25_22_d;
wire [ 3:0] op_21_20_d;
wire [31:0] op_19_15_d;
wire [31:0] op_14_10_d; //rk;
wire [31:0] op_9_5_d  ; //rj;
wire [31:0] op_4_0_d  ; //rd;

assign op_31_26 = fs_inst[31:26];
assign op_25_22 = fs_inst[25:22];
assign op_21_20 = fs_inst[21:20];
assign op_19_15 = fs_inst[19:15];
assign op_14_10 = fs_inst[14:10];
assign op_9_5   = fs_inst[ 9: 5];
assign op_4_0   = fs_inst[ 4: 0];

wire csrxchg_helper = op_9_5[4:1] != 4'b0;

decoder_6_64 u_dec0(.in(op_31_26 ), .out(op_31_26_d ));
decoder_4_16 u_dec1(.in(op_25_22 ), .out(op_25_22_d ));
decoder_2_4  u_dec2(.in(op_21_20 ), .out(op_21_20_d ));
decoder_5_32 u_dec3(.in(op_19_15 ), .out(op_19_15_d ));
decoder_5_32 u_dec4(.in(op_14_10 ), .out(op_14_10_d ));
decoder_5_32 u_dec5(.in(op_9_5   ), .out(op_9_5_d   ));
decoder_5_32 u_dec6(.in(op_4_0   ), .out(op_4_0_d   ));

reg [`PS_TO_FS_BUS_WD -1:0] ps_to_fs_bus_r;
reg ex_ertn_wrong_r;

wire [31:0] fs_inst;
reg  [31:0] inst_r;
reg         inst_r_valid;

wire ws_ertn, ws_ex;
assign {ws_ertn, ws_ex} = ws_to_fs_bus;

wire fs_ex;
wire [5:0] fs_ecode;
wire s0_refill_ex, s0_ex;

assign {s0_ex,
		s0_refill_ex,
		fs_ecode,
		fs_ex,
		fs_pc
		} = ps_to_fs_bus_r;

assign fs_to_ds_bus = {csrxchg_helper, //286
					   op_31_26_d, //285:222
					   op_25_22_d, //221:206
					   op_21_20_d, //205:202
					   op_19_15_d, //201:170
					   op_14_10_d, //169:138
					   op_9_5_d  , //137:106
					   op_4_0_d  , //105:74
					   fs_is_icacop, //73
					   s0_ex,        //72
					   s0_refill_ex, //71
					   fs_ecode,  //70:65
					   fs_ex   ,  //64:64
					   fs_inst ,  //63:32
                       fs_pc       //31:0
					   };

assign fs_ready_go    = fs_ex || (data_ok || inst_r_valid) && !next_data_ok_is_wrong;
assign fs_allowin     = !fs_valid || fs_ready_go && ds_allowin;
assign fs_to_ds_valid =  fs_valid && fs_ready_go && !ws_ex && !ws_ertn && !es_br_taken && !wrongPC_br && (!br_bus_r_valid || fs_ex) && !refetch;
always @(posedge clk) begin
    if (reset) begin
        fs_valid <= 1'b0;
		ps_to_fs_bus_r <= `PS_TO_FS_BUS_WD'b0;
		inst_r_valid <= 1'b0;
		inst_r <= 32'b0;
		ex_ertn_wrong_r <= 1'b0;
		wrong_data_ok <= 1'b0;
    end
    else begin
		if (fs_allowin) begin
			fs_valid <= ps_to_fs_valid;
		end
 		else if (ws_ex || ws_ertn || refetch || es_br_taken) begin
			fs_valid <= 1'b0;
		end
		
		if (ps_to_fs_valid && fs_allowin) begin
			ps_to_fs_bus_r <= ps_to_fs_bus;
		end
		
		if (data_ok) begin
			wrong_data_ok <= 1'b0;
		end
		else if ((ws_ex || ws_ertn) && fs_valid && !fs_ready_go) begin
			wrong_data_ok <= 1'b1;
		end
		
		if ( (fs_ready_go && !ds_allowin) && !inst_r_valid) begin
			inst_r <= fs_inst;
			inst_r_valid <= 1'b1;
		end
		else if ( (ds_allowin && fs_ready_go) || ws_ex || ws_ertn) begin
			inst_r_valid <= 1'b0;
		end
		
		if (data_ok) begin
			ex_ertn_wrong_r <= 1'b0;
		end
		else if ((ws_ex || ws_ertn) && (ps_to_fs_valid || (!fs_allowin || !fs_ready_go))) begin
			ex_ertn_wrong_r <= 1'b1;
		end
    end
end

assign fs_inst = inst_r_valid ? inst_r : inst_sram_rdata;

assign fs_is_icacop = fs_valid && (data_ok && inst_sram_rdata[31:22] == 10'b0000011000 && inst_sram_rdata[2:0] == 3'b000)
				   || inst_r_valid && (inst_r[31:22] == 10'b0000011000 && inst_r[2:0] == 3'b000);

endmodule