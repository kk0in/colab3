/*
 * Generated by Bluespec Compiler, version 2021.12.1 (build fd50140)
 * 
 * On Sun Apr  3 18:50:29 KST 2022
 * 
 */

/* Generation options: */
#ifndef __mkTbFftPipelined_h__
#define __mkTbFftPipelined_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"
#include "mkFftPipelined.h"
#include "mkFftCombinational.h"


/* Class declaration for the mkTbFftPipelined module */
class MOD_mkTbFftPipelined : public Module {
 
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
  MOD_Reg<tUInt8> INST_feed_count;
  MOD_mkFftPipelined INST_fft;
  MOD_mkFftCombinational INST_fft_comb;
  MOD_Reg<tUInt8> INST_randomVal1_0_init;
  MOD_Reg<tUInt8> INST_randomVal1_10_init;
  MOD_Reg<tUInt8> INST_randomVal1_11_init;
  MOD_Reg<tUInt8> INST_randomVal1_12_init;
  MOD_Reg<tUInt8> INST_randomVal1_13_init;
  MOD_Reg<tUInt8> INST_randomVal1_14_init;
  MOD_Reg<tUInt8> INST_randomVal1_15_init;
  MOD_Reg<tUInt8> INST_randomVal1_16_init;
  MOD_Reg<tUInt8> INST_randomVal1_17_init;
  MOD_Reg<tUInt8> INST_randomVal1_18_init;
  MOD_Reg<tUInt8> INST_randomVal1_19_init;
  MOD_Reg<tUInt8> INST_randomVal1_1_init;
  MOD_Reg<tUInt8> INST_randomVal1_20_init;
  MOD_Reg<tUInt8> INST_randomVal1_21_init;
  MOD_Reg<tUInt8> INST_randomVal1_22_init;
  MOD_Reg<tUInt8> INST_randomVal1_23_init;
  MOD_Reg<tUInt8> INST_randomVal1_24_init;
  MOD_Reg<tUInt8> INST_randomVal1_25_init;
  MOD_Reg<tUInt8> INST_randomVal1_26_init;
  MOD_Reg<tUInt8> INST_randomVal1_27_init;
  MOD_Reg<tUInt8> INST_randomVal1_28_init;
  MOD_Reg<tUInt8> INST_randomVal1_29_init;
  MOD_Reg<tUInt8> INST_randomVal1_2_init;
  MOD_Reg<tUInt8> INST_randomVal1_30_init;
  MOD_Reg<tUInt8> INST_randomVal1_31_init;
  MOD_Reg<tUInt8> INST_randomVal1_32_init;
  MOD_Reg<tUInt8> INST_randomVal1_33_init;
  MOD_Reg<tUInt8> INST_randomVal1_34_init;
  MOD_Reg<tUInt8> INST_randomVal1_35_init;
  MOD_Reg<tUInt8> INST_randomVal1_36_init;
  MOD_Reg<tUInt8> INST_randomVal1_37_init;
  MOD_Reg<tUInt8> INST_randomVal1_38_init;
  MOD_Reg<tUInt8> INST_randomVal1_39_init;
  MOD_Reg<tUInt8> INST_randomVal1_3_init;
  MOD_Reg<tUInt8> INST_randomVal1_40_init;
  MOD_Reg<tUInt8> INST_randomVal1_41_init;
  MOD_Reg<tUInt8> INST_randomVal1_42_init;
  MOD_Reg<tUInt8> INST_randomVal1_43_init;
  MOD_Reg<tUInt8> INST_randomVal1_44_init;
  MOD_Reg<tUInt8> INST_randomVal1_45_init;
  MOD_Reg<tUInt8> INST_randomVal1_46_init;
  MOD_Reg<tUInt8> INST_randomVal1_47_init;
  MOD_Reg<tUInt8> INST_randomVal1_48_init;
  MOD_Reg<tUInt8> INST_randomVal1_49_init;
  MOD_Reg<tUInt8> INST_randomVal1_4_init;
  MOD_Reg<tUInt8> INST_randomVal1_50_init;
  MOD_Reg<tUInt8> INST_randomVal1_51_init;
  MOD_Reg<tUInt8> INST_randomVal1_52_init;
  MOD_Reg<tUInt8> INST_randomVal1_53_init;
  MOD_Reg<tUInt8> INST_randomVal1_54_init;
  MOD_Reg<tUInt8> INST_randomVal1_55_init;
  MOD_Reg<tUInt8> INST_randomVal1_56_init;
  MOD_Reg<tUInt8> INST_randomVal1_57_init;
  MOD_Reg<tUInt8> INST_randomVal1_58_init;
  MOD_Reg<tUInt8> INST_randomVal1_59_init;
  MOD_Reg<tUInt8> INST_randomVal1_5_init;
  MOD_Reg<tUInt8> INST_randomVal1_60_init;
  MOD_Reg<tUInt8> INST_randomVal1_61_init;
  MOD_Reg<tUInt8> INST_randomVal1_62_init;
  MOD_Reg<tUInt8> INST_randomVal1_63_init;
  MOD_Reg<tUInt8> INST_randomVal1_6_init;
  MOD_Reg<tUInt8> INST_randomVal1_7_init;
  MOD_Reg<tUInt8> INST_randomVal1_8_init;
  MOD_Reg<tUInt8> INST_randomVal1_9_init;
  MOD_Reg<tUInt8> INST_randomVal2_0_init;
  MOD_Reg<tUInt8> INST_randomVal2_10_init;
  MOD_Reg<tUInt8> INST_randomVal2_11_init;
  MOD_Reg<tUInt8> INST_randomVal2_12_init;
  MOD_Reg<tUInt8> INST_randomVal2_13_init;
  MOD_Reg<tUInt8> INST_randomVal2_14_init;
  MOD_Reg<tUInt8> INST_randomVal2_15_init;
  MOD_Reg<tUInt8> INST_randomVal2_16_init;
  MOD_Reg<tUInt8> INST_randomVal2_17_init;
  MOD_Reg<tUInt8> INST_randomVal2_18_init;
  MOD_Reg<tUInt8> INST_randomVal2_19_init;
  MOD_Reg<tUInt8> INST_randomVal2_1_init;
  MOD_Reg<tUInt8> INST_randomVal2_20_init;
  MOD_Reg<tUInt8> INST_randomVal2_21_init;
  MOD_Reg<tUInt8> INST_randomVal2_22_init;
  MOD_Reg<tUInt8> INST_randomVal2_23_init;
  MOD_Reg<tUInt8> INST_randomVal2_24_init;
  MOD_Reg<tUInt8> INST_randomVal2_25_init;
  MOD_Reg<tUInt8> INST_randomVal2_26_init;
  MOD_Reg<tUInt8> INST_randomVal2_27_init;
  MOD_Reg<tUInt8> INST_randomVal2_28_init;
  MOD_Reg<tUInt8> INST_randomVal2_29_init;
  MOD_Reg<tUInt8> INST_randomVal2_2_init;
  MOD_Reg<tUInt8> INST_randomVal2_30_init;
  MOD_Reg<tUInt8> INST_randomVal2_31_init;
  MOD_Reg<tUInt8> INST_randomVal2_32_init;
  MOD_Reg<tUInt8> INST_randomVal2_33_init;
  MOD_Reg<tUInt8> INST_randomVal2_34_init;
  MOD_Reg<tUInt8> INST_randomVal2_35_init;
  MOD_Reg<tUInt8> INST_randomVal2_36_init;
  MOD_Reg<tUInt8> INST_randomVal2_37_init;
  MOD_Reg<tUInt8> INST_randomVal2_38_init;
  MOD_Reg<tUInt8> INST_randomVal2_39_init;
  MOD_Reg<tUInt8> INST_randomVal2_3_init;
  MOD_Reg<tUInt8> INST_randomVal2_40_init;
  MOD_Reg<tUInt8> INST_randomVal2_41_init;
  MOD_Reg<tUInt8> INST_randomVal2_42_init;
  MOD_Reg<tUInt8> INST_randomVal2_43_init;
  MOD_Reg<tUInt8> INST_randomVal2_44_init;
  MOD_Reg<tUInt8> INST_randomVal2_45_init;
  MOD_Reg<tUInt8> INST_randomVal2_46_init;
  MOD_Reg<tUInt8> INST_randomVal2_47_init;
  MOD_Reg<tUInt8> INST_randomVal2_48_init;
  MOD_Reg<tUInt8> INST_randomVal2_49_init;
  MOD_Reg<tUInt8> INST_randomVal2_4_init;
  MOD_Reg<tUInt8> INST_randomVal2_50_init;
  MOD_Reg<tUInt8> INST_randomVal2_51_init;
  MOD_Reg<tUInt8> INST_randomVal2_52_init;
  MOD_Reg<tUInt8> INST_randomVal2_53_init;
  MOD_Reg<tUInt8> INST_randomVal2_54_init;
  MOD_Reg<tUInt8> INST_randomVal2_55_init;
  MOD_Reg<tUInt8> INST_randomVal2_56_init;
  MOD_Reg<tUInt8> INST_randomVal2_57_init;
  MOD_Reg<tUInt8> INST_randomVal2_58_init;
  MOD_Reg<tUInt8> INST_randomVal2_59_init;
  MOD_Reg<tUInt8> INST_randomVal2_5_init;
  MOD_Reg<tUInt8> INST_randomVal2_60_init;
  MOD_Reg<tUInt8> INST_randomVal2_61_init;
  MOD_Reg<tUInt8> INST_randomVal2_62_init;
  MOD_Reg<tUInt8> INST_randomVal2_63_init;
  MOD_Reg<tUInt8> INST_randomVal2_6_init;
  MOD_Reg<tUInt8> INST_randomVal2_7_init;
  MOD_Reg<tUInt8> INST_randomVal2_8_init;
  MOD_Reg<tUInt8> INST_randomVal2_9_init;
  MOD_Reg<tUInt8> INST_stream_count;
 
 /* Constructor */
 public:
  MOD_mkTbFftPipelined(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_x__h56213;
  tUInt8 DEF_x__h56252;
 
 /* Local definitions */
 private:
  tUInt64 DEF_x__h43693;
  tUInt64 DEF_x__h43493;
  tUInt64 DEF_x__h43270;
  tUInt64 DEF_x__h43070;
  tUInt64 DEF_x__h42847;
  tUInt64 DEF_x__h42647;
  tUInt64 DEF_x__h42424;
  tUInt64 DEF_x__h42224;
  tUInt64 DEF_x__h42001;
  tUInt64 DEF_x__h41801;
  tUInt64 DEF_x__h41578;
  tUInt64 DEF_x__h41378;
  tUInt64 DEF_x__h41155;
  tUInt64 DEF_x__h40955;
  tUInt64 DEF_x__h40732;
  tUInt64 DEF_x__h40532;
  tUInt64 DEF_x__h40309;
  tUInt64 DEF_x__h40109;
  tUInt64 DEF_x__h39886;
  tUInt64 DEF_x__h39686;
  tUInt64 DEF_x__h39463;
  tUInt64 DEF_x__h39263;
  tUInt64 DEF_x__h39040;
  tUInt64 DEF_x__h38840;
  tUInt64 DEF_x__h38617;
  tUInt64 DEF_x__h38417;
  tUInt64 DEF_x__h38194;
  tUInt64 DEF_x__h37994;
  tUInt64 DEF_x__h37771;
  tUInt64 DEF_x__h37571;
  tUInt64 DEF_x__h37348;
  tUInt64 DEF_x__h37148;
  tUInt64 DEF_x__h36925;
  tUInt64 DEF_x__h36725;
  tUInt64 DEF_x__h36502;
  tUInt64 DEF_x__h36302;
  tUInt64 DEF_x__h36079;
  tUInt64 DEF_x__h35879;
  tUInt64 DEF_x__h35656;
  tUInt64 DEF_x__h35456;
  tUInt64 DEF_x__h35233;
  tUInt64 DEF_x__h35033;
  tUInt64 DEF_x__h34810;
  tUInt64 DEF_x__h34610;
  tUInt64 DEF_x__h34387;
  tUInt64 DEF_x__h34187;
  tUInt64 DEF_x__h33964;
  tUInt64 DEF_x__h33764;
  tUInt64 DEF_x__h33541;
  tUInt64 DEF_x__h33341;
  tUInt64 DEF_x__h33118;
  tUInt64 DEF_x__h32918;
  tUInt64 DEF_x__h32695;
  tUInt64 DEF_x__h32495;
  tUInt64 DEF_x__h32272;
  tUInt64 DEF_x__h32072;
  tUInt64 DEF_x__h31849;
  tUInt64 DEF_x__h31649;
  tUInt64 DEF_x__h31426;
  tUInt64 DEF_x__h31226;
  tUInt64 DEF_x__h31003;
  tUInt64 DEF_x__h30803;
  tUInt64 DEF_x__h30580;
  tUInt64 DEF_x__h30380;
  tUInt64 DEF_x__h30157;
  tUInt64 DEF_x__h29957;
  tUInt64 DEF_x__h29734;
  tUInt64 DEF_x__h29534;
  tUInt64 DEF_x__h29311;
  tUInt64 DEF_x__h29111;
  tUInt64 DEF_x__h28888;
  tUInt64 DEF_x__h28688;
  tUInt64 DEF_x__h28465;
  tUInt64 DEF_x__h28265;
  tUInt64 DEF_x__h28042;
  tUInt64 DEF_x__h27842;
  tUInt64 DEF_x__h27619;
  tUInt64 DEF_x__h27419;
  tUInt64 DEF_x__h27196;
  tUInt64 DEF_x__h26996;
  tUInt64 DEF_x__h26773;
  tUInt64 DEF_x__h26573;
  tUInt64 DEF_x__h26350;
  tUInt64 DEF_x__h26150;
  tUInt64 DEF_x__h25927;
  tUInt64 DEF_x__h25727;
  tUInt64 DEF_x__h25504;
  tUInt64 DEF_x__h25304;
  tUInt64 DEF_x__h25081;
  tUInt64 DEF_x__h24881;
  tUInt64 DEF_x__h24658;
  tUInt64 DEF_x__h24458;
  tUInt64 DEF_x__h24235;
  tUInt64 DEF_x__h24035;
  tUInt64 DEF_x__h23812;
  tUInt64 DEF_x__h23612;
  tUInt64 DEF_x__h23389;
  tUInt64 DEF_x__h23189;
  tUInt64 DEF_x__h22966;
  tUInt64 DEF_x__h22766;
  tUInt64 DEF_x__h22543;
  tUInt64 DEF_x__h22343;
  tUInt64 DEF_x__h22120;
  tUInt64 DEF_x__h21920;
  tUInt64 DEF_x__h21697;
  tUInt64 DEF_x__h21497;
  tUInt64 DEF_x__h21274;
  tUInt64 DEF_x__h21074;
  tUInt64 DEF_x__h20851;
  tUInt64 DEF_x__h20651;
  tUInt64 DEF_x__h20428;
  tUInt64 DEF_x__h20228;
  tUInt64 DEF_x__h20005;
  tUInt64 DEF_x__h19805;
  tUInt64 DEF_x__h19582;
  tUInt64 DEF_x__h19382;
  tUInt64 DEF_x__h19159;
  tUInt64 DEF_x__h18959;
  tUInt64 DEF_x__h18736;
  tUInt64 DEF_x__h18536;
  tUInt64 DEF_x__h18313;
  tUInt64 DEF_x__h18113;
  tUInt64 DEF_x__h17890;
  tUInt64 DEF_x__h17690;
  tUInt64 DEF_x__h17467;
  tUInt64 DEF_x__h17267;
  tUInt64 DEF_x__h17044;
  tUInt64 DEF_x__h15099;
  tUWide DEF_fft_comb_deq___d716;
  tUWide DEF_fft_deq___d718;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d709;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d699;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d689;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d679;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d669;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d659;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d649;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d639;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d629;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d619;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d609;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d599;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d589;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d579;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d569;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d559;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d549;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d539;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d529;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d519;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d509;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d499;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d489;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d479;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d469;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d459;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d449;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d439;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d429;
  tUWide DEF_TASK_getRandom_91_BITS_7_TO_0_92_CONCAT_TASK_g_ETC___d419;
 
 /* Rules */
 public:
  void RL_randomVal1_0_initialize();
  void RL_randomVal1_1_initialize();
  void RL_randomVal1_2_initialize();
  void RL_randomVal1_3_initialize();
  void RL_randomVal1_4_initialize();
  void RL_randomVal1_5_initialize();
  void RL_randomVal1_6_initialize();
  void RL_randomVal1_7_initialize();
  void RL_randomVal1_8_initialize();
  void RL_randomVal1_9_initialize();
  void RL_randomVal1_10_initialize();
  void RL_randomVal1_11_initialize();
  void RL_randomVal1_12_initialize();
  void RL_randomVal1_13_initialize();
  void RL_randomVal1_14_initialize();
  void RL_randomVal1_15_initialize();
  void RL_randomVal1_16_initialize();
  void RL_randomVal1_17_initialize();
  void RL_randomVal1_18_initialize();
  void RL_randomVal1_19_initialize();
  void RL_randomVal1_20_initialize();
  void RL_randomVal1_21_initialize();
  void RL_randomVal1_22_initialize();
  void RL_randomVal1_23_initialize();
  void RL_randomVal1_24_initialize();
  void RL_randomVal1_25_initialize();
  void RL_randomVal1_26_initialize();
  void RL_randomVal1_27_initialize();
  void RL_randomVal1_28_initialize();
  void RL_randomVal1_29_initialize();
  void RL_randomVal1_30_initialize();
  void RL_randomVal1_31_initialize();
  void RL_randomVal1_32_initialize();
  void RL_randomVal1_33_initialize();
  void RL_randomVal1_34_initialize();
  void RL_randomVal1_35_initialize();
  void RL_randomVal1_36_initialize();
  void RL_randomVal1_37_initialize();
  void RL_randomVal1_38_initialize();
  void RL_randomVal1_39_initialize();
  void RL_randomVal1_40_initialize();
  void RL_randomVal1_41_initialize();
  void RL_randomVal1_42_initialize();
  void RL_randomVal1_43_initialize();
  void RL_randomVal1_44_initialize();
  void RL_randomVal1_45_initialize();
  void RL_randomVal1_46_initialize();
  void RL_randomVal1_47_initialize();
  void RL_randomVal1_48_initialize();
  void RL_randomVal1_49_initialize();
  void RL_randomVal1_50_initialize();
  void RL_randomVal1_51_initialize();
  void RL_randomVal1_52_initialize();
  void RL_randomVal1_53_initialize();
  void RL_randomVal1_54_initialize();
  void RL_randomVal1_55_initialize();
  void RL_randomVal1_56_initialize();
  void RL_randomVal1_57_initialize();
  void RL_randomVal1_58_initialize();
  void RL_randomVal1_59_initialize();
  void RL_randomVal1_60_initialize();
  void RL_randomVal1_61_initialize();
  void RL_randomVal1_62_initialize();
  void RL_randomVal1_63_initialize();
  void RL_randomVal2_0_initialize();
  void RL_randomVal2_1_initialize();
  void RL_randomVal2_2_initialize();
  void RL_randomVal2_3_initialize();
  void RL_randomVal2_4_initialize();
  void RL_randomVal2_5_initialize();
  void RL_randomVal2_6_initialize();
  void RL_randomVal2_7_initialize();
  void RL_randomVal2_8_initialize();
  void RL_randomVal2_9_initialize();
  void RL_randomVal2_10_initialize();
  void RL_randomVal2_11_initialize();
  void RL_randomVal2_12_initialize();
  void RL_randomVal2_13_initialize();
  void RL_randomVal2_14_initialize();
  void RL_randomVal2_15_initialize();
  void RL_randomVal2_16_initialize();
  void RL_randomVal2_17_initialize();
  void RL_randomVal2_18_initialize();
  void RL_randomVal2_19_initialize();
  void RL_randomVal2_20_initialize();
  void RL_randomVal2_21_initialize();
  void RL_randomVal2_22_initialize();
  void RL_randomVal2_23_initialize();
  void RL_randomVal2_24_initialize();
  void RL_randomVal2_25_initialize();
  void RL_randomVal2_26_initialize();
  void RL_randomVal2_27_initialize();
  void RL_randomVal2_28_initialize();
  void RL_randomVal2_29_initialize();
  void RL_randomVal2_30_initialize();
  void RL_randomVal2_31_initialize();
  void RL_randomVal2_32_initialize();
  void RL_randomVal2_33_initialize();
  void RL_randomVal2_34_initialize();
  void RL_randomVal2_35_initialize();
  void RL_randomVal2_36_initialize();
  void RL_randomVal2_37_initialize();
  void RL_randomVal2_38_initialize();
  void RL_randomVal2_39_initialize();
  void RL_randomVal2_40_initialize();
  void RL_randomVal2_41_initialize();
  void RL_randomVal2_42_initialize();
  void RL_randomVal2_43_initialize();
  void RL_randomVal2_44_initialize();
  void RL_randomVal2_45_initialize();
  void RL_randomVal2_46_initialize();
  void RL_randomVal2_47_initialize();
  void RL_randomVal2_48_initialize();
  void RL_randomVal2_49_initialize();
  void RL_randomVal2_50_initialize();
  void RL_randomVal2_51_initialize();
  void RL_randomVal2_52_initialize();
  void RL_randomVal2_53_initialize();
  void RL_randomVal2_54_initialize();
  void RL_randomVal2_55_initialize();
  void RL_randomVal2_56_initialize();
  void RL_randomVal2_57_initialize();
  void RL_randomVal2_58_initialize();
  void RL_randomVal2_59_initialize();
  void RL_randomVal2_60_initialize();
  void RL_randomVal2_61_initialize();
  void RL_randomVal2_62_initialize();
  void RL_randomVal2_63_initialize();
  void RL_feed();
  void RL_stream();
  void RL_pass();
 
 /* Methods */
 public:
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkTbFftPipelined &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkTbFftPipelined &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkTbFftPipelined &backing);
  void vcd_submodules(tVCDDumpType dt, unsigned int levels, MOD_mkTbFftPipelined &backing);
};

#endif /* ifndef __mkTbFftPipelined_h__ */
