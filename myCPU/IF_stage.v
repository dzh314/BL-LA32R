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
	//from ds
	input                          ds_to_fs_bus   ,
	//from ws
	input  [`WS_TO_FS_BUS_WD -1:0] ws_to_fs_bus   ,
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
wire        fs_allowin;
wire        ps_to_fs_valid;
wire        br_taken;
reg         wrong_data_ok; //已经发送了请求，还未收到data_ok，发生ws_ex
wire        next_data_ok_is_wrong;
assign      next_data_ok_is_wrong = wrong_data_ok || wrong_req_r;

assign br_taken = ds_to_fs_bus;

reg [`PS_TO_FS_BUS_WD -1:0] ps_to_fs_bus_r;
reg ex_ertn_wrong_r;

wire [31:0] fs_inst;
wire [31:0] fs_pc;
reg  [31:0] inst_r;
reg         inst_r_valid;

wire ws_ertn, ws_ex;
assign {ws_ertn, ws_ex} = ws_to_fs_bus;

wire fs_ex;
wire [5:0] fs_ecode;
wire s0_refill_ex, s0_ex;

wire fs_is_icacop;

assign {s0_ex,
		s0_refill_ex,
		fs_ecode,
		fs_ex,
		fs_pc
		} = ps_to_fs_bus_r;

assign fs_to_ds_bus = {fs_is_icacop, //73
					   s0_ex,        //72
					   s0_refill_ex, //71
					   fs_ecode,  //70:65
					   fs_ex   ,  //64:64
					   fs_inst ,  //63:32
                       fs_pc       //31:0
					   };

assign fs_ready_go    = fs_ex || (data_ok || inst_r_valid) && !next_data_ok_is_wrong;
assign fs_allowin     = !fs_valid || fs_ready_go && ds_allowin;
assign fs_to_ds_valid =  fs_valid && fs_ready_go && !br_taken && !ws_ex && !ws_ertn && !wrongPC_br && (!br_bus_r_valid || fs_ex) && !refetch;
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
 		else if (ws_ex || ws_ertn || refetch) begin
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
