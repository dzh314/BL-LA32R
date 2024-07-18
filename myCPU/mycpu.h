`ifndef MYCPU_H
    `define MYCPU_H

    `define BR_BUS_WD        33
	`define PS_TO_FS_BUS_WD  42
    `define FS_TO_DS_BUS_WD  74
    `define DS_TO_ES_BUS_WD  231
    `define ES_TO_MS_BUS_WD  171
    `define MS_TO_WS_BUS_WD  163

    `define WS_TO_RF_BUS_WD  38
	
	`define ES_TO_DS_BUS_WD  43
	`define MS_TO_DS_BUS_WD  42
	`define WS_TO_DS_BUS_WD  42
	`define WS_TO_PS_BUS_WD  118
	`define WS_TO_FS_BUS_WD  2

	`define TLBNUM           16
	`define TLB_TO_WS_BUS_WD 94
	`define WS_TO_TLB_BUS_WD 185
	`define TLB_TO_PS_BUS_WD 3
	`define TLB_TO_ES_BUS_WD 4
`endif