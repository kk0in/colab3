/*
 * Generated by Bluespec Compiler, version 2021.12.1 (build fd50140)
 * 
 * On Sun Apr  3 18:50:29 KST 2022
 * 
 */

/* Generation options: */
#ifndef __mkFftPipelined_h__
#define __mkFftPipelined_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"
#include "mkBfly4.h"


/* Class declaration for the mkFftPipelined module */
class MOD_mkFftPipelined : public Module {
 
 /* Clock handles */
 private:
  tClock __clk_handle_0;
 
 /* Clock gate handles */
 public:
  tUInt8 *clk_gate[0];
 
 /* Instantiation parameters */
 public:
 
 /* Module state */
 public:
  MOD_mkBfly4 INST_bfly_0_0;
  MOD_mkBfly4 INST_bfly_0_1;
  MOD_mkBfly4 INST_bfly_0_10;
  MOD_mkBfly4 INST_bfly_0_11;
  MOD_mkBfly4 INST_bfly_0_12;
  MOD_mkBfly4 INST_bfly_0_13;
  MOD_mkBfly4 INST_bfly_0_14;
  MOD_mkBfly4 INST_bfly_0_15;
  MOD_mkBfly4 INST_bfly_0_2;
  MOD_mkBfly4 INST_bfly_0_3;
  MOD_mkBfly4 INST_bfly_0_4;
  MOD_mkBfly4 INST_bfly_0_5;
  MOD_mkBfly4 INST_bfly_0_6;
  MOD_mkBfly4 INST_bfly_0_7;
  MOD_mkBfly4 INST_bfly_0_8;
  MOD_mkBfly4 INST_bfly_0_9;
  MOD_mkBfly4 INST_bfly_1_0;
  MOD_mkBfly4 INST_bfly_1_1;
  MOD_mkBfly4 INST_bfly_1_10;
  MOD_mkBfly4 INST_bfly_1_11;
  MOD_mkBfly4 INST_bfly_1_12;
  MOD_mkBfly4 INST_bfly_1_13;
  MOD_mkBfly4 INST_bfly_1_14;
  MOD_mkBfly4 INST_bfly_1_15;
  MOD_mkBfly4 INST_bfly_1_2;
  MOD_mkBfly4 INST_bfly_1_3;
  MOD_mkBfly4 INST_bfly_1_4;
  MOD_mkBfly4 INST_bfly_1_5;
  MOD_mkBfly4 INST_bfly_1_6;
  MOD_mkBfly4 INST_bfly_1_7;
  MOD_mkBfly4 INST_bfly_1_8;
  MOD_mkBfly4 INST_bfly_1_9;
  MOD_mkBfly4 INST_bfly_2_0;
  MOD_mkBfly4 INST_bfly_2_1;
  MOD_mkBfly4 INST_bfly_2_10;
  MOD_mkBfly4 INST_bfly_2_11;
  MOD_mkBfly4 INST_bfly_2_12;
  MOD_mkBfly4 INST_bfly_2_13;
  MOD_mkBfly4 INST_bfly_2_14;
  MOD_mkBfly4 INST_bfly_2_15;
  MOD_mkBfly4 INST_bfly_2_2;
  MOD_mkBfly4 INST_bfly_2_3;
  MOD_mkBfly4 INST_bfly_2_4;
  MOD_mkBfly4 INST_bfly_2_5;
  MOD_mkBfly4 INST_bfly_2_6;
  MOD_mkBfly4 INST_bfly_2_7;
  MOD_mkBfly4 INST_bfly_2_8;
  MOD_mkBfly4 INST_bfly_2_9;
  MOD_Reg<tUWide> INST_inFifo_data_0;
  MOD_Reg<tUWide> INST_inFifo_data_1;
  MOD_Reg<tUInt8> INST_inFifo_deqEn_dummy2_0;
  MOD_Reg<tUInt8> INST_inFifo_deqEn_dummy2_1;
  MOD_Reg<tUInt8> INST_inFifo_deqEn_dummy2_2;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_0_0;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_0_1;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_0_2;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_1_0;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_1_1;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_1_2;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_2_0;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_2_1;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_2_2;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_lat_0;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_lat_1;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_lat_2;
  MOD_Reg<tUInt8> INST_inFifo_deqEn_rl;
  MOD_Reg<tUInt8> INST_inFifo_deqP_dummy2_0;
  MOD_Reg<tUInt8> INST_inFifo_deqP_dummy2_1;
  MOD_Reg<tUInt8> INST_inFifo_deqP_dummy2_2;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_0_2;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_1_2;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_2_0;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_2_1;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_2_2;
  MOD_Wire<tUInt8> INST_inFifo_deqP_lat_0;
  MOD_Wire<tUInt8> INST_inFifo_deqP_lat_1;
  MOD_Wire<tUInt8> INST_inFifo_deqP_lat_2;
  MOD_Reg<tUInt8> INST_inFifo_deqP_rl;
  MOD_Reg<tUInt8> INST_inFifo_enqEn_dummy2_0;
  MOD_Reg<tUInt8> INST_inFifo_enqEn_dummy2_1;
  MOD_Reg<tUInt8> INST_inFifo_enqEn_dummy2_2;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_0_0;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_0_1;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_0_2;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_1_0;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_1_1;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_1_2;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_2_0;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_2_1;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_2_2;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_lat_0;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_lat_1;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_lat_2;
  MOD_Reg<tUInt8> INST_inFifo_enqEn_rl;
  MOD_Reg<tUInt8> INST_inFifo_enqP_dummy2_0;
  MOD_Reg<tUInt8> INST_inFifo_enqP_dummy2_1;
  MOD_Reg<tUInt8> INST_inFifo_enqP_dummy2_2;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_0_2;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_1_2;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_2_0;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_2_1;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_2_2;
  MOD_Wire<tUInt8> INST_inFifo_enqP_lat_0;
  MOD_Wire<tUInt8> INST_inFifo_enqP_lat_1;
  MOD_Wire<tUInt8> INST_inFifo_enqP_lat_2;
  MOD_Reg<tUInt8> INST_inFifo_enqP_rl;
  MOD_Reg<tUInt8> INST_inFifo_tempData_dummy2_0;
  MOD_Reg<tUInt8> INST_inFifo_tempData_dummy2_1;
  MOD_Wire<tUWide> INST_inFifo_tempData_dummy_0_0;
  MOD_Wire<tUWide> INST_inFifo_tempData_dummy_0_1;
  MOD_Wire<tUWide> INST_inFifo_tempData_dummy_1_0;
  MOD_Wire<tUWide> INST_inFifo_tempData_dummy_1_1;
  MOD_Wire<tUWide> INST_inFifo_tempData_lat_0;
  MOD_Wire<tUWide> INST_inFifo_tempData_lat_1;
  MOD_Reg<tUWide> INST_inFifo_tempData_rl;
  MOD_Reg<tUInt8> INST_inFifo_tempEnqP_dummy2_0;
  MOD_Reg<tUInt8> INST_inFifo_tempEnqP_dummy2_1;
  MOD_Wire<tUInt8> INST_inFifo_tempEnqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_inFifo_tempEnqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_inFifo_tempEnqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_inFifo_tempEnqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_inFifo_tempEnqP_lat_0;
  MOD_Wire<tUInt8> INST_inFifo_tempEnqP_lat_1;
  MOD_Reg<tUInt8> INST_inFifo_tempEnqP_rl;
  MOD_Reg<tUWide> INST_outFifo_data_0;
  MOD_Reg<tUWide> INST_outFifo_data_1;
  MOD_Reg<tUInt8> INST_outFifo_deqEn_dummy2_0;
  MOD_Reg<tUInt8> INST_outFifo_deqEn_dummy2_1;
  MOD_Reg<tUInt8> INST_outFifo_deqEn_dummy2_2;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_0_0;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_0_1;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_0_2;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_1_0;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_1_1;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_1_2;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_2_0;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_2_1;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_2_2;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_lat_0;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_lat_1;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_lat_2;
  MOD_Reg<tUInt8> INST_outFifo_deqEn_rl;
  MOD_Reg<tUInt8> INST_outFifo_deqP_dummy2_0;
  MOD_Reg<tUInt8> INST_outFifo_deqP_dummy2_1;
  MOD_Reg<tUInt8> INST_outFifo_deqP_dummy2_2;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_0_2;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_1_2;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_2_0;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_2_1;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_2_2;
  MOD_Wire<tUInt8> INST_outFifo_deqP_lat_0;
  MOD_Wire<tUInt8> INST_outFifo_deqP_lat_1;
  MOD_Wire<tUInt8> INST_outFifo_deqP_lat_2;
  MOD_Reg<tUInt8> INST_outFifo_deqP_rl;
  MOD_Reg<tUInt8> INST_outFifo_enqEn_dummy2_0;
  MOD_Reg<tUInt8> INST_outFifo_enqEn_dummy2_1;
  MOD_Reg<tUInt8> INST_outFifo_enqEn_dummy2_2;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_0_0;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_0_1;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_0_2;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_1_0;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_1_1;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_1_2;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_2_0;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_2_1;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_2_2;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_lat_0;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_lat_1;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_lat_2;
  MOD_Reg<tUInt8> INST_outFifo_enqEn_rl;
  MOD_Reg<tUInt8> INST_outFifo_enqP_dummy2_0;
  MOD_Reg<tUInt8> INST_outFifo_enqP_dummy2_1;
  MOD_Reg<tUInt8> INST_outFifo_enqP_dummy2_2;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_0_2;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_1_2;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_2_0;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_2_1;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_2_2;
  MOD_Wire<tUInt8> INST_outFifo_enqP_lat_0;
  MOD_Wire<tUInt8> INST_outFifo_enqP_lat_1;
  MOD_Wire<tUInt8> INST_outFifo_enqP_lat_2;
  MOD_Reg<tUInt8> INST_outFifo_enqP_rl;
  MOD_Reg<tUInt8> INST_outFifo_tempData_dummy2_0;
  MOD_Reg<tUInt8> INST_outFifo_tempData_dummy2_1;
  MOD_Wire<tUWide> INST_outFifo_tempData_dummy_0_0;
  MOD_Wire<tUWide> INST_outFifo_tempData_dummy_0_1;
  MOD_Wire<tUWide> INST_outFifo_tempData_dummy_1_0;
  MOD_Wire<tUWide> INST_outFifo_tempData_dummy_1_1;
  MOD_Wire<tUWide> INST_outFifo_tempData_lat_0;
  MOD_Wire<tUWide> INST_outFifo_tempData_lat_1;
  MOD_Reg<tUWide> INST_outFifo_tempData_rl;
  MOD_Reg<tUInt8> INST_outFifo_tempEnqP_dummy2_0;
  MOD_Reg<tUInt8> INST_outFifo_tempEnqP_dummy2_1;
  MOD_Wire<tUInt8> INST_outFifo_tempEnqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_outFifo_tempEnqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_outFifo_tempEnqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_outFifo_tempEnqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_outFifo_tempEnqP_lat_0;
  MOD_Wire<tUInt8> INST_outFifo_tempEnqP_lat_1;
  MOD_Reg<tUInt8> INST_outFifo_tempEnqP_rl;
  MOD_Reg<tUWide> INST_sReg1;
  MOD_Reg<tUWide> INST_sReg2;
 
 /* Constructor */
 public:
  MOD_mkFftPipelined(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
  tUWide PORT_enq_in;
  tUWide PORT_deq;
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_NOT_inFifo_deqEn_rl_3___d89;
  tUInt8 DEF_outFifo_deqEn_rl__h83361;
  tUInt8 DEF_inFifo_enqEn_rl__h32626;
  tUInt8 DEF_outFifo_enqEn_rl__h82031;
  tUInt8 DEF_inFifo_deqEn_rl__h33959;
  tUWide DEF_sReg2___d249;
  tUInt8 DEF_sReg2_49_BIT_1024___d250;
  tUInt8 DEF_IF_inFifo_deqEn_dummy2_0_42_AND_inFifo_deqEn_d_ETC___d247;
 
 /* Local definitions */
 private:
  tUInt8 DEF_outFifo_tempEnqP_lat_0_whas____d177;
  tUInt8 DEF_outFifo_deqEn_lat_1_whas____d160;
  tUInt8 DEF_outFifo_deqEn_lat_0_whas____d162;
  tUInt8 DEF_outFifo_enqEn_lat_1_whas____d150;
  tUInt8 DEF_outFifo_enqEn_lat_0_whas____d152;
  tUInt8 DEF_inFifo_tempEnqP_lat_0_whas____d56;
  tUInt8 DEF_inFifo_deqEn_lat_1_whas____d39;
  tUInt8 DEF_inFifo_deqEn_lat_0_whas____d41;
  tUInt8 DEF_inFifo_enqEn_lat_1_whas____d29;
  tUInt8 DEF_inFifo_enqEn_lat_0_whas____d31;
  tUWide DEF_outFifo_tempData_lat_1_wget____d169;
  tUWide DEF_outFifo_tempData_lat_0_wget____d171;
  tUWide DEF_inFifo_tempData_lat_1_wget____d48;
  tUWide DEF_inFifo_tempData_lat_0_wget____d50;
  tUInt8 DEF_outFifo_tempEnqP_rl___d179;
  tUInt8 DEF_outFifo_tempEnqP_lat_0_wget____d178;
  tUInt8 DEF_inFifo_tempEnqP_rl___d58;
  tUInt8 DEF_inFifo_tempEnqP_lat_0_wget____d57;
  tUInt8 DEF_outFifo_deqEn_lat_1_wget____d161;
  tUInt8 DEF_outFifo_deqEn_lat_0_wget____d163;
  tUInt8 DEF_outFifo_enqEn_lat_1_wget____d151;
  tUInt8 DEF_outFifo_enqEn_lat_0_wget____d153;
  tUInt8 DEF_inFifo_deqEn_lat_1_wget____d40;
  tUInt8 DEF_inFifo_deqEn_lat_0_wget____d42;
  tUInt8 DEF_inFifo_enqEn_lat_1_wget____d30;
  tUInt8 DEF_inFifo_enqEn_lat_0_wget____d32;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d937;
  tUWide DEF_IF_outFifo_tempData_lat_1_whas__68_THEN_outFif_ETC___d174;
  tUWide DEF_IF_outFifo_tempData_lat_0_whas__70_THEN_outFif_ETC___d173;
  tUWide DEF_IF_inFifo_tempData_lat_0_whas__9_THEN_inFifo_t_ETC___d52;
  tUWide DEF_IF_inFifo_tempData_lat_1_whas__7_THEN_inFifo_t_ETC___d53;
  tUInt8 DEF_IF_outFifo_deqP_lat_1_whas__38_THEN_IF_outFifo_ETC___d146;
  tUInt8 DEF_IF_outFifo_enqP_lat_1_whas__25_THEN_IF_outFifo_ETC___d133;
  tUInt8 DEF_IF_inFifo_deqP_lat_1_whas__7_THEN_IF_inFifo_de_ETC___d25;
  tUInt8 DEF_IF_inFifo_enqP_lat_1_whas_THEN_IF_inFifo_enqP__ETC___d12;
  tUInt8 DEF__0_CONCAT_DONTCARE___d120;
  tUWide DEF_outFifo_tempData_rl__h71960;
  tUWide DEF_inFifo_tempData_rl__h22552;
  tUInt8 DEF_x_wget__h51404;
  tUInt8 DEF_x_wget__h51355;
  tUInt8 DEF_x_wget__h49842;
  tUInt8 DEF_x_wget__h49793;
  tUInt8 DEF_x_wget__h1985;
  tUInt8 DEF_x_wget__h1936;
  tUInt8 DEF_x_wget__h420;
  tUInt8 DEF_x_wget__h371;
  tUInt8 DEF_upd__h52696;
  tUInt8 DEF_upd__h52669;
  tUInt8 DEF_upd__h51138;
  tUInt8 DEF_upd__h51111;
  tUInt8 DEF_upd__h3277;
  tUInt8 DEF_upd__h3250;
  tUInt8 DEF_upd__h1719;
  tUInt8 DEF_upd__h1692;
  tUWide DEF_sReg1___d943;
  tUWide DEF_outFifo_data_1__h775630;
  tUWide DEF_outFifo_data_0__h775605;
  tUWide DEF_inFifo_data_1__h142734;
  tUWide DEF_inFifo_data_0__h142709;
  tUInt8 DEF_upd__h765506;
  tUInt8 DEF_upd__h753543;
  tUInt8 DEF_upd__h110543;
  tUInt8 DEF_upd__h762517;
  tUWide DEF_sReg1_43_BIT_1024_44_CONCAT_bfly_1_15_bfly4_22_ETC___d1072;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1070;
  tUWide DEF_inFifo_deqEn_rl_3_CONCAT_bfly_0_15_bfly4_22515_ETC___d939;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1200;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1197;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d934;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1067;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1194;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1793;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1775;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d931;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1064;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1191;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1757;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d928;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1061;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1188;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1739;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d925;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1058;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1185;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1721;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d922;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1055;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1182;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1703;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d919;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1052;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1179;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1685;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d916;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1049;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1176;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1667;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d913;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1046;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1173;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1649;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d910;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1043;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1170;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1631;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d907;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1040;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1167;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1613;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d904;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1037;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1164;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1595;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d901;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1034;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1161;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1577;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d898;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1031;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1158;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1559;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d895;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1028;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1155;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1541;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d892;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1025;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1152;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1523;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d889;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1022;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1147;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1505;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d850;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1017;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1142;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1487;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d811;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1012;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1137;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1469;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d772;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1007;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1132;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1451;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d733;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d1002;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1127;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1433;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d694;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d997;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1122;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1415;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d655;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d992;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1117;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1397;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d616;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d987;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1112;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1379;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d577;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d982;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1107;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1361;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d538;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d977;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1102;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1343;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d499;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d972;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1097;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1325;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d460;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d967;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1092;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1307;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d421;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d962;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_sReg2_49_B_ETC___d1087;
  tUWide DEF_SEL_ARR_outFifo_data_0_233_BITS_1023_TO_1016_2_ETC___d1289;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d382;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_sReg1_43_B_ETC___d957;
 
 /* Rules */
 public:
  void RL_inFifo_enqP_canon();
  void RL_inFifo_deqP_canon();
  void RL_inFifo_enqEn_canon();
  void RL_inFifo_deqEn_canon();
  void RL_inFifo_tempData_canon();
  void RL_inFifo_tempEnqP_canon();
  void RL_inFifo_canonicalize();
  void RL_outFifo_enqP_canon();
  void RL_outFifo_deqP_canon();
  void RL_outFifo_enqEn_canon();
  void RL_outFifo_deqEn_canon();
  void RL_outFifo_tempData_canon();
  void RL_outFifo_tempEnqP_canon();
  void RL_outFifo_canonicalize();
  void RL_doFft();
 
 /* Methods */
 public:
  void METH_enq(tUWide ARG_enq_in);
  tUInt8 METH_RDY_enq();
  tUWide METH_deq();
  tUInt8 METH_RDY_deq();
 
 /* Reset routines */
 public:
  void reset_RST_N(tUInt8 ARG_rst_in);
 
 /* Static handles to reset routines */
 public:
 
 /* Pointers to reset fns in parent module for asserting output resets */
 private:
 
 /* Functions for the parent module to register its reset fns */
 public:
 
 /* Functions to set the elaborated clock id */
 public:
  void set_clk_0(char const *s);
 
 /* State dumping routine */
 public:
  void dump_state(unsigned int indent);
 
 /* VCD dumping routines */
 public:
  unsigned int dump_VCD_defs(unsigned int levels);
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkFftPipelined &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkFftPipelined &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkFftPipelined &backing);
  void vcd_submodules(tVCDDumpType dt, unsigned int levels, MOD_mkFftPipelined &backing);
};

#endif /* ifndef __mkFftPipelined_h__ */
