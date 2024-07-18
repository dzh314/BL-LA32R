`include "mycpu.h"

module tlb
(
    input  wire                      clk,

    // search port 0 (for fetch)
    input  wire [               18:0] s0_vppn    ,
    input  wire                       s0_va_bit12,
    input  wire [                9:0] s0_asid    ,
    output wire                       s0_found   ,
    output wire [$clog2(`TLBNUM)-1:0] s0_index   , // 4位索引，[3:0]
    output wire [               19:0] s0_ppn     ,
    output wire [                5:0] s0_ps      ,
    output wire [                1:0] s0_plv     ,
    output wire [                1:0] s0_mat     ,
    output wire                       s0_d       ,
    output wire                       s0_v       ,

    // search port 1 (for load/store)
    input  wire [               18:0] s1_vppn    ,
    input  wire                       s1_va_bit12,
    input  wire [                9:0] s1_asid    ,
    output wire                       s1_found   ,
    output wire [$clog2(`TLBNUM)-1:0] s1_index   ,
    output wire [               19:0] s1_ppn     ,
    output wire [                5:0] s1_ps      ,
    output wire [                1:0] s1_plv     ,
    output wire [                1:0] s1_mat     ,
    output wire                       s1_d       ,
    output wire                       s1_v       ,

    // invtlb opcode
    input  wire                      invtlb_valid,
    input  wire [               4:0] invtlb_op   ,

    // write port
    input  wire                       we     ,
    input  wire [$clog2(`TLBNUM)-1:0] w_index,
    input  wire                       w_e    ,
    input  wire [               18:0] w_vppn ,
    input  wire [                5:0] w_ps   ,
    input  wire [                9:0] w_asid ,
    input  wire                       w_g    ,
    input  wire [               19:0] w_ppn0 ,
    input  wire [                1:0] w_plv0 ,
    input  wire [                1:0] w_mat0 ,
    input  wire                       w_d0   ,
    input  wire                       w_v0   ,
    input  wire [               19:0] w_ppn1 ,
    input  wire [                1:0] w_plv1 ,
    input  wire [                1:0] w_mat1 ,
    input  wire                       w_d1   ,
    input  wire                       w_v1   ,

    // read port
    input  wire [$clog2(`TLBNUM)-1:0] r_index,
    output wire                       r_e    ,
    output wire [               18:0] r_vppn ,
    output wire [                5:0] r_ps   ,
    output wire [                9:0] r_asid ,
    output wire                       r_g    ,
    output wire [               19:0] r_ppn0 ,
    output wire [                1:0] r_plv0 ,
    output wire [                1:0] r_mat0 ,
    output wire                       r_d0   ,
    output wire                       r_v0   ,
    output wire [               19:0] r_ppn1 ,
    output wire [                1:0] r_plv1 ,
    output wire [                1:0] r_mat1 ,
    output wire                       r_d1   ,
    output wire                       r_v1
);

reg  [`TLBNUM-1:0] tlb_e                     ;
reg  [`TLBNUM-1:0] tlb_ps2MB                 ; //pagesize 1:4MB, 0:4KB
reg  [        5:0] tlb_ps       [`TLBNUM-1:0];
reg  [       18:0] tlb_vppn     [`TLBNUM-1:0];
reg  [        9:0] tlb_asid     [`TLBNUM-1:0];
reg                tlb_g        [`TLBNUM-1:0];
reg  [       19:0] tlb_ppn0     [`TLBNUM-1:0];
reg  [        1:0] tlb_plv0     [`TLBNUM-1:0];
reg  [        1:0] tlb_mat0     [`TLBNUM-1:0];
reg                tlb_d0       [`TLBNUM-1:0];
reg                tlb_v0       [`TLBNUM-1:0];
reg  [       19:0] tlb_ppn1     [`TLBNUM-1:0];
reg  [        1:0] tlb_plv1     [`TLBNUM-1:0];
reg  [        1:0] tlb_mat1     [`TLBNUM-1:0];
reg                tlb_d1       [`TLBNUM-1:0];
reg                tlb_v1       [`TLBNUM-1:0];
wire [`TLBNUM-1:0] match0                    ;
wire [`TLBNUM-1:0] match1                    ;


//search
generate
    for (genvar i = 0; i < `TLBNUM; i = i + 1) begin : match_gen0
        assign match0[i] = (s0_vppn[18:9]==tlb_vppn[i][18:9])
                 && (tlb_ps2MB[i] || s0_vppn[8:0]==tlb_vppn[i][8:0])
                 && ((s0_asid==tlb_asid[i]) || tlb_g[i])
                 && tlb_e[i];
    end
endgenerate
/* assign match0[ 0] = (s0_vppn[18:9]==tlb_vppn[ 0][18:9])
                 && (tlb_ps2MB[ 0] || s0_vppn[8:0]==tlb_vppn[ 0][8:0])
                 && ((s0_asid==tlb_asid[ 0]) || tlb_g[ 0])
                 && tlb_e[0];
assign match0[ 1] = (s0_vppn[18:9]==tlb_vppn[ 1][18:9])
                 && (tlb_ps2MB[ 1] || s0_vppn[8:0]==tlb_vppn[ 1][8:0])
                 && ((s0_asid==tlb_asid[ 1]) || tlb_g[ 1])
                 && tlb_e[1];
assign match0[ 2] = (s0_vppn[18:9]==tlb_vppn[ 2][18:9])
                 && (tlb_ps2MB[ 2] || s0_vppn[8:0]==tlb_vppn[ 2][8:0])
                 && ((s0_asid==tlb_asid[ 2]) || tlb_g[ 1])
                 && tlb_e[2];
assign match0[ 3] = (s0_vppn[18:9]==tlb_vppn[ 3][18:9])
                 && (tlb_ps2MB[ 3] || s0_vppn[8:0]==tlb_vppn[ 3][8:0])
                 && ((s0_asid==tlb_asid[ 3]) || tlb_g[ 3])
                 && tlb_e[3];
assign match0[ 4] = (s0_vppn[18:9]==tlb_vppn[ 4][18:9])
                 && (tlb_ps2MB[ 4] || s0_vppn[8:0]==tlb_vppn[ 4][8:0])
                 && ((s0_asid==tlb_asid[ 4]) || tlb_g[ 4])
                 && tlb_e[4];
assign match0[ 5] = (s0_vppn[18:9]==tlb_vppn[ 5][18:9])
                 && (tlb_ps2MB[ 5] || s0_vppn[8:0]==tlb_vppn[ 5][8:0])
                 && ((s0_asid==tlb_asid[ 5]) || tlb_g[ 5])
                 && tlb_e[5];
assign match0[ 6] = (s0_vppn[18:9]==tlb_vppn[ 6][18:9])
                 && (tlb_ps2MB[ 6] || s0_vppn[8:0]==tlb_vppn[ 6][8:0])
                 && ((s0_asid==tlb_asid[ 6]) || tlb_g[ 6])
                 && tlb_e[6];
assign match0[ 7] = (s0_vppn[18:9]==tlb_vppn[ 7][18:9])
                 && (tlb_ps2MB[ 7] || s0_vppn[8:0]==tlb_vppn[ 7][8:0])
                 && ((s0_asid==tlb_asid[ 7]) || tlb_g[ 7])
                 && tlb_e[7];
assign match0[ 8] = (s0_vppn[18:9]==tlb_vppn[ 8][18:9])
                 && (tlb_ps2MB[ 8] || s0_vppn[8:0]==tlb_vppn[ 8][8:0])
                 && ((s0_asid==tlb_asid[ 8]) || tlb_g[ 8])
                 && tlb_e[8];
assign match0[ 9] = (s0_vppn[18:9]==tlb_vppn[ 9][18:9])
                 && (tlb_ps2MB[ 9] || s0_vppn[8:0]==tlb_vppn[ 9][8:0])
                 && ((s0_asid==tlb_asid[ 9]) || tlb_g[ 9])
                 && tlb_e[9];
assign match0[10] = (s0_vppn[18:9]==tlb_vppn[10][18:9])
                 && (tlb_ps2MB[10] || s0_vppn[8:0]==tlb_vppn[10][8:0])
                 && ((s0_asid==tlb_asid[10]) || tlb_g[10])
                 && tlb_e[10];
assign match0[11] = (s0_vppn[18:9]==tlb_vppn[11][18:9])
                 && (tlb_ps2MB[11] || s0_vppn[8:0]==tlb_vppn[11][8:0])
                 && ((s0_asid==tlb_asid[11]) || tlb_g[11])
                 && tlb_e[11];
assign match0[12] = (s0_vppn[18:9]==tlb_vppn[12][18:9])
                 && (tlb_ps2MB[12] || s0_vppn[8:0]==tlb_vppn[12][8:0])
                 && ((s0_asid==tlb_asid[12]) || tlb_g[12])
                 && tlb_e[12];
assign match0[13] = (s0_vppn[18:9]==tlb_vppn[13][18:9])
                 && (tlb_ps2MB[13] || s0_vppn[8:0]==tlb_vppn[13][8:0])
                 && ((s0_asid==tlb_asid[13]) || tlb_g[13])
                 && tlb_e[13];
assign match0[14] = (s0_vppn[18:9]==tlb_vppn[14][18:9])
                 && (tlb_ps2MB[14] || s0_vppn[8:0]==tlb_vppn[14][8:0])
                 && ((s0_asid==tlb_asid[14]) || tlb_g[14])
                 && tlb_e[14];
assign match0[15] = (s0_vppn[18:9]==tlb_vppn[15][18:9])
                 && (tlb_ps2MB[15] || s0_vppn[8:0]==tlb_vppn[15][8:0])
                 && ((s0_asid==tlb_asid[15]) || tlb_g[15])
                 && tlb_e[15]; */
				 
assign match1[ 0] = (s1_vppn[18:9]==tlb_vppn[ 0][18:9])
                 && (tlb_ps2MB[ 0] || s1_vppn[8:0]==tlb_vppn[ 0][8:0])
                 && ((s1_asid==tlb_asid[ 0]) || tlb_g[ 0])
                 && tlb_e[0];
assign match1[ 1] = (s1_vppn[18:9]==tlb_vppn[ 1][18:9])
                 && (tlb_ps2MB[ 1] || s1_vppn[8:0]==tlb_vppn[ 1][8:0])
                 && ((s1_asid==tlb_asid[ 1]) || tlb_g[ 1])
                 && tlb_e[1];
assign match1[ 2] = (s1_vppn[18:9]==tlb_vppn[ 2][18:9])
                 && (tlb_ps2MB[ 2] || s1_vppn[8:0]==tlb_vppn[ 2][8:0])
                 && ((s1_asid==tlb_asid[ 2]) || tlb_g[ 2])
                 && tlb_e[2];
assign match1[ 3] = (s1_vppn[18:9]==tlb_vppn[ 3][18:9])
                 && (tlb_ps2MB[ 3] || s1_vppn[8:0]==tlb_vppn[ 3][8:0])
                 && ((s1_asid==tlb_asid[ 3]) || tlb_g[ 3])
                 && tlb_e[3];
assign match1[ 4] = (s1_vppn[18:9]==tlb_vppn[ 4][18:9])
                 && (tlb_ps2MB[ 4] || s1_vppn[8:0]==tlb_vppn[ 4][8:0])
                 && ((s1_asid==tlb_asid[ 4]) || tlb_g[ 4])
                 && tlb_e[4];
assign match1[ 5] = (s1_vppn[18:9]==tlb_vppn[ 5][18:9])
                 && (tlb_ps2MB[ 5] || s1_vppn[8:0]==tlb_vppn[ 5][8:0])
                 && ((s1_asid==tlb_asid[ 5]) || tlb_g[ 5])
                 && tlb_e[5];
assign match1[ 6] = (s1_vppn[18:9]==tlb_vppn[ 6][18:9])
                 && (tlb_ps2MB[ 6] || s1_vppn[8:0]==tlb_vppn[ 6][8:0])
                 && ((s1_asid==tlb_asid[ 6]) || tlb_g[ 6])
                 && tlb_e[6];
assign match1[ 7] = (s1_vppn[18:9]==tlb_vppn[ 7][18:9])
                 && (tlb_ps2MB[ 7] || s1_vppn[8:0]==tlb_vppn[ 7][8:0])
                 && ((s1_asid==tlb_asid[ 7]) || tlb_g[ 7])
                 && tlb_e[7];
assign match1[ 8] = (s1_vppn[18:9]==tlb_vppn[ 8][18:9])
                 && (tlb_ps2MB[ 8] || s1_vppn[8:0]==tlb_vppn[ 8][8:0])
                 && ((s1_asid==tlb_asid[ 8]) || tlb_g[ 8])
                 && tlb_e[8];
assign match1[ 9] = (s1_vppn[18:9]==tlb_vppn[ 9][18:9])
                 && (tlb_ps2MB[ 9] || s1_vppn[8:0]==tlb_vppn[ 9][8:0])
                 && ((s1_asid==tlb_asid[ 9]) || tlb_g[ 9])
                 && tlb_e[9];
assign match1[10] = (s1_vppn[18:9]==tlb_vppn[10][18:9])
                 && (tlb_ps2MB[10] || s1_vppn[8:0]==tlb_vppn[10][8:0])
                 && ((s1_asid==tlb_asid[10]) || tlb_g[10])
                 && tlb_e[10];
assign match1[11] = (s1_vppn[18:9]==tlb_vppn[11][18:9])
                 && (tlb_ps2MB[11] || s1_vppn[8:0]==tlb_vppn[11][8:0])
                 && ((s1_asid==tlb_asid[11]) || tlb_g[11])
                 && tlb_e[11];
assign match1[12] = (s1_vppn[18:9]==tlb_vppn[12][18:9])
                 && (tlb_ps2MB[12] || s1_vppn[8:0]==tlb_vppn[12][8:0])
                 && ((s1_asid==tlb_asid[12]) || tlb_g[12])
                 && tlb_e[12];
assign match1[13] = (s1_vppn[18:9]==tlb_vppn[13][18:9])
                 && (tlb_ps2MB[13] || s1_vppn[8:0]==tlb_vppn[13][8:0])
                 && ((s1_asid==tlb_asid[13]) || tlb_g[13])
                 && tlb_e[13];
assign match1[14] = (s1_vppn[18:9]==tlb_vppn[14][18:9])
                 && (tlb_ps2MB[14] || s1_vppn[8:0]==tlb_vppn[14][8:0])
                 && ((s1_asid==tlb_asid[14]) || tlb_g[14])
                 && tlb_e[14];
assign match1[15] = (s1_vppn[18:9]==tlb_vppn[15][18:9])
                 && (tlb_ps2MB[15] || s1_vppn[8:0]==tlb_vppn[15][8:0])
                 && ((s1_asid==tlb_asid[15]) || tlb_g[15])
                 && tlb_e[15];

assign s0_found     = | match0;
assign s0_index     = s0_found ? (match0[ 0] ? 4'h0 : (match0[ 1] ? 4'h1 : (
                                  match0[ 2] ? 4'h2 : (match0[ 3] ? 4'h3 : (
                                  match0[ 4] ? 4'h4 : (match0[ 5] ? 4'h5 : (
                                  match0[ 6] ? 4'h6 : (match0[ 7] ? 4'h7 : (
                                  match0[ 8] ? 4'h8 : (match0[ 9] ? 4'h9 : (
                                  match0[10] ? 4'ha : (match0[11] ? 4'hb : (
                                  match0[12] ? 4'hc : (match0[13] ? 4'hd : (
                                  match0[14] ? 4'he : (match0[15] ? 4'hf : 4'hx
                                  )))))))))))))))) : 4'hx;
wire even_or_odd0 = tlb_ps2MB[s0_index] ? s0_vppn[8] : s0_va_bit12;
assign s0_ppn		= s0_found ? (even_or_odd0 ? tlb_ppn1[s0_index] : tlb_ppn0[s0_index]) : 20'hxxx;
assign s0_ps        = tlb_ps[s0_index]  ;
assign s0_plv       = even_or_odd0 ? tlb_plv1[s0_index] : tlb_plv0[s0_index];
assign s0_mat       = even_or_odd0 ? tlb_mat1[s0_index] : tlb_mat0[s0_index];
assign s0_d         = even_or_odd0 ? tlb_d1[s0_index] : tlb_d0[s0_index];
assign s0_v         = even_or_odd0 ? tlb_v1[s0_index] : tlb_v0[s0_index];

assign s1_found     = match1[ 0] | match1[ 1] | match1[ 2] | match1[ 3]
                    | match1[ 4] | match1[ 5] | match1[ 6] | match1[ 7]
                    | match1[ 8] | match1[ 9] | match1[10] | match1[11]
                    | match1[12] | match1[13] | match1[14] | match1[15];
assign s1_index     = s1_found ? (match1[ 0] ? 4'h0 : (match1[ 1] ? 4'h1 : (
                                  match1[ 2] ? 4'h2 : (match1[ 3] ? 4'h3 : (
                                  match1[ 4] ? 4'h4 : (match1[ 5] ? 4'h5 : (
                                  match1[ 6] ? 4'h6 : (match1[ 7] ? 4'h7 : (
                                  match1[ 8] ? 4'h8 : (match1[ 9] ? 4'h9 : (
                                  match1[10] ? 4'ha : (match1[11] ? 4'hb : (
                                  match1[12] ? 4'hc : (match1[13] ? 4'hd : (
                                  match1[14] ? 4'he : (match1[15] ? 4'hf : 4'hx
                                  )))))))))))))))) : 4'hx;
wire even_or_odd1 = tlb_ps2MB[s1_index] ? s1_vppn[8] : s1_va_bit12;
assign s1_ppn		= s1_found ? (even_or_odd1 ? tlb_ppn1[s1_index] : tlb_ppn0[s1_index]) : 20'hxxx;
assign s1_ps        =   tlb_ps[s1_index];
assign s1_plv       =   even_or_odd1 ? tlb_plv1[s1_index] : tlb_plv0[s1_index];
assign s1_mat       =   even_or_odd1 ? tlb_mat1[s1_index] : tlb_mat0[s1_index];
assign s1_d         =   even_or_odd1 ? tlb_d1[s1_index] : tlb_d0[s1_index];
assign s1_v         =   even_or_odd1 ? tlb_v1[s1_index] : tlb_v0[s1_index];

//invtlb
integer i;
always @(posedge clk) begin
    if(invtlb_valid) begin
        if(invtlb_op == 5'b00000||invtlb_op == 5'b00001) begin
            for(i = 0; i < `TLBNUM; i = i+1) begin
                tlb_e[i] <= 1'b0;
            end
        end
        else if(invtlb_op == 5'b00010) begin
            for(i = 0; i < `TLBNUM; i = i+1) begin
                if(tlb_g[i] == 1'b1) begin
                    tlb_e[i] <= 1'b0;
                end
            end
        end
        else if(invtlb_op == 5'b00011) begin
            for(i = 0; i < `TLBNUM; i = i+1) begin
                if(tlb_g[i]==1'b0) begin
                    tlb_e[i] <= 1'b0;
                end
            end
        end
        else if(invtlb_op == 5'b00100) begin
            for(i = 0; i < `TLBNUM; i = i+1) begin
                if(tlb_g[i]==1'b0 && tlb_asid[i]==s1_asid) begin
                    tlb_e[i] <= 1'b0;
                end
            end
        end
        else if(invtlb_op == 5'b00101) begin
            for(i = 0; i < `TLBNUM; i = i+1) begin
                if(tlb_g[i]==1'b0 && tlb_asid[i]==s1_asid && tlb_vppn[i][18:10]==s1_vppn[18:10] && (tlb_ps2MB[i] || s1_vppn[9:0] == tlb_vppn[i][9:0])) begin
                    tlb_e[i] <= 1'b0;
                end
            end
        end
        else if(invtlb_op == 5'b00110) begin
            for(i = 0; i < `TLBNUM; i = i+1) begin
                if((tlb_g[i]==1'b1 || tlb_asid[i]==s1_asid) && tlb_vppn[i][18:10]==s1_vppn[18:10] && (tlb_ps2MB[i] || s1_vppn[9:0] == tlb_vppn[i][9:0])) begin
                    tlb_e[i] <= 1'b0;
                end
            end
        end
    end    
end

//write
always @(posedge clk) begin
    if(we) begin
        tlb_e[w_index]       <= w_e                    ;
        tlb_ps[w_index]      <= w_ps                   ;
        tlb_ps2MB[w_index]   <= (w_ps == 6'h15) ? 1 : 0; 
        tlb_vppn[w_index]    <= w_vppn                 ;
        tlb_asid[w_index]    <= w_asid                 ;
        tlb_g[w_index]       <= w_g                    ;
        tlb_ppn0[w_index]    <= w_ppn0                 ;
        tlb_plv0[w_index]    <= w_plv0                 ;
        tlb_mat0[w_index]    <= w_mat0                 ;
        tlb_d0[w_index]      <= w_d0                   ;
        tlb_v0[w_index]      <= w_v0                   ;
        tlb_ppn1[w_index]    <= w_ppn1                 ;
        tlb_plv1[w_index]    <= w_plv1                 ;
        tlb_mat1[w_index]    <= w_mat1                 ;
        tlb_d1[w_index]      <= w_d1                   ;
        tlb_v1[w_index]      <= w_v1                   ;
    end
end

//read
assign r_e      = tlb_e[r_index]   ;
assign r_vppn   = tlb_vppn[r_index];
assign r_ps     = tlb_ps[r_index]  ;
assign r_asid   = tlb_asid[r_index];
assign r_g      = tlb_g[r_index]   ;
assign r_ppn0   = tlb_ppn0[r_index];
assign r_plv0   = tlb_plv0[r_index];
assign r_mat0   = tlb_mat0[r_index];
assign r_d0     = tlb_d0[r_index]  ;
assign r_v0     = tlb_v0[r_index]  ;
assign r_ppn1   = tlb_ppn1[r_index];
assign r_plv1   = tlb_plv1[r_index];
assign r_mat1   = tlb_mat1[r_index];
assign r_d1     = tlb_d1[r_index]  ;
assign r_v1     = tlb_v1[r_index]  ;

endmodule