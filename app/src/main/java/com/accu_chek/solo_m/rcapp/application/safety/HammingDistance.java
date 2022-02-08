/**
 * ===========================================================================
 * Copyright 2014 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.demo.safety.HammingDistance
 * Brief: 
 *
 * Create Date: 1/6/2015
 * $Revision: 24179 $
 * $Author: StanleySu $
 * $Id: HammingDistance.java 24179 2015-11-16 09:39:13Z StanleySu $
 */

package com.accu_chek.solo_m.rcapp.application.safety;

public final class HammingDistance
{
    
    public static final int SAFETY_BOOLEAN_TRUE = 0xD4;
    
    public static final int SAFETY_BOOLEAN_FALSE = 0x2B;
    
    public static final int SAFETY_NUMBER_UINT8_01 = 0x0F;
    
    public static final int SAFETY_NUMBER_UINT8_02 = 0x33;
    
    public static final int SAFETY_NUMBER_UINT8_03 = 0x3C;
    
    public static final int SAFETY_NUMBER_UINT8_04 = 0x55;
    
    public static final int SAFETY_NUMBER_UINT8_05 = 0x5A;
    
    public static final int SAFETY_NUMBER_UINT8_06 = 0x66;
    
    public static final int SAFETY_NUMBER_UINT8_07 = 0x69;
    
    public static final int SAFETY_NUMBER_UINT8_08 = 0x96;
    
    public static final int SAFETY_NUMBER_UINT8_09 = 0x99;
    
    public static final int SAFETY_NUMBER_UINT8_10 = 0xA5;
    
    public static final int SAFETY_NUMBER_UINT8_11 = 0xAA;
    
    public static final int SAFETY_NUMBER_UINT8_12 = 0xC3;
    
    public static final int SAFETY_NUMBER_UINT8_13 = 0xCC;
    
    public static final int SAFETY_NUMBER_UINT8_14 = 0xF0;
    
    public static final int SAFETY_NUMBER_UINT8_15 = 0xFF;
    
    public static final int SAFETY_NUMBER_UINT16_0001 = 0x0F00;
    
    public static final int SAFETY_NUMBER_UINT16_0002 = 0x3300;
    
    public static final int SAFETY_NUMBER_UINT16_0003 = 0x3C00;
    
    public static final int SAFETY_NUMBER_UINT16_0004 = 0x5500;
    
    public static final int SAFETY_NUMBER_UINT16_0005 = 0x5A00;
    
    public static final int SAFETY_NUMBER_UINT16_0006 = 0x6600;
    
    public static final int SAFETY_NUMBER_UINT16_0007 = 0x6900;
    
    public static final int SAFETY_NUMBER_UINT16_0008 = 0x9600;
    
    public static final int SAFETY_NUMBER_UINT16_0009 = 0x9900;
    
    public static final int SAFETY_NUMBER_UINT16_0010 = 0xA500;
    
    public static final int SAFETY_NUMBER_UINT16_0011 = 0xAA00;
    
    public static final int SAFETY_NUMBER_UINT16_0012 = 0xC300;
    
    public static final int SAFETY_NUMBER_UINT16_0013 = 0xCC00;
    
    public static final int SAFETY_NUMBER_UINT16_0014 = 0xF000;
    
    public static final int SAFETY_NUMBER_UINT16_0015 = 0xFF00;

    public static final int SAFETY_NUMBER_VALUE_0001 = 0x000F;
    
    public static final int SAFETY_NUMBER_VALUE_0002 = 0x0033;
    
    public static final int SAFETY_NUMBER_VALUE_0003 = 0x003C;
    
    public static final int SAFETY_NUMBER_VALUE_0004 = 0x0055;
    
    public static final int SAFETY_NUMBER_VALUE_0005 = 0x005A;
    
    public static final int SAFETY_NUMBER_VALUE_0006 = 0x0066;
    
    public static final int SAFETY_NUMBER_VALUE_0007 = 0x0069;
    
    public static final int SAFETY_NUMBER_VALUE_0008 = 0x0096;
    
    public static final int SAFETY_NUMBER_VALUE_0009 = 0x0099;
    
    public static final int SAFETY_NUMBER_VALUE_0010 = 0x00A5;
    
    public static final int SAFETY_NUMBER_VALUE_0011 = 0x00AA;
    
    public static final int SAFETY_NUMBER_VALUE_0012 = 0x00C3;
    
    public static final int SAFETY_NUMBER_VALUE_0013 = 0x00CC;
    
    public static final int SAFETY_NUMBER_VALUE_0014 = 0x00F0;
    
    public static final int SAFETY_NUMBER_VALUE_0015 = 0x00FF;
    
    public static final int SAFETY_NUMBER_VALUE_0016 = 0x0303;
    
    public static final int SAFETY_NUMBER_VALUE_0017 = 0x030C;
    
    public static final int SAFETY_NUMBER_VALUE_0018 = 0x0330;
    
    public static final int SAFETY_NUMBER_VALUE_0019 = 0x033F;
    
    public static final int SAFETY_NUMBER_VALUE_0020 = 0x0356;
    
    public static final int SAFETY_NUMBER_VALUE_0021 = 0x0359;
    
    public static final int SAFETY_NUMBER_VALUE_0022 = 0x0365;
    
    public static final int SAFETY_NUMBER_VALUE_0023 = 0x036A;
    
    public static final int SAFETY_NUMBER_VALUE_0024 = 0x0395;
    
    public static final int SAFETY_NUMBER_VALUE_0025 = 0x039A;
    
    public static final int SAFETY_NUMBER_VALUE_0026 = 0x03A6;
    
    public static final int SAFETY_NUMBER_VALUE_0027 = 0x03A9;
    
    public static final int SAFETY_NUMBER_VALUE_0028 = 0x03C0;
    
    public static final int SAFETY_NUMBER_VALUE_0029 = 0x03CF;
    
    public static final int SAFETY_NUMBER_VALUE_0030 = 0x03F3;
    
    public static final int SAFETY_NUMBER_VALUE_0031 = 0x03FC;
    
    public static final int SAFETY_NUMBER_VALUE_0032 = 0x0505;
    
    public static final int SAFETY_NUMBER_VALUE_0033 = 0x050A;
    
    public static final int SAFETY_NUMBER_VALUE_0034 = 0x0536;
    
    public static final int SAFETY_NUMBER_VALUE_0035 = 0x0539;
    
    public static final int SAFETY_NUMBER_VALUE_0036 = 0x0550;
    
    public static final int SAFETY_NUMBER_VALUE_0037 = 0x055F;
    
    public static final int SAFETY_NUMBER_VALUE_0038 = 0x0563;
    
    public static final int SAFETY_NUMBER_VALUE_0039 = 0x056C;
    
    public static final int SAFETY_NUMBER_VALUE_0040 = 0x0593;
    
    public static final int SAFETY_NUMBER_VALUE_0041 = 0x059C;
    
    public static final int SAFETY_NUMBER_VALUE_0042 = 0x05A0;
    
    public static final int SAFETY_NUMBER_VALUE_0043 = 0x05AF;
    
    public static final int SAFETY_NUMBER_VALUE_0044 = 0x05C6;
    
    public static final int SAFETY_NUMBER_VALUE_0045 = 0x05C9;
    
    public static final int SAFETY_NUMBER_VALUE_0046 = 0x05F5;
    
    public static final int SAFETY_NUMBER_VALUE_0047 = 0x05FA;
    
    public static final int SAFETY_NUMBER_VALUE_0048 = 0x0606;
    
    public static final int SAFETY_NUMBER_VALUE_0049 = 0x0609;
    
    public static final int SAFETY_NUMBER_VALUE_0050 = 0x0635;
    
    public static final int SAFETY_NUMBER_VALUE_0051 = 0x063A;
    
    public static final int SAFETY_NUMBER_VALUE_0052 = 0x0653;
    
    public static final int SAFETY_NUMBER_VALUE_0053 = 0x065C;
    
    public static final int SAFETY_NUMBER_VALUE_0054 = 0x0660;
    
    public static final int SAFETY_NUMBER_VALUE_0055 = 0x066F;
    
    public static final int SAFETY_NUMBER_VALUE_0056 = 0x0690;
    
    public static final int SAFETY_NUMBER_VALUE_0057 = 0x069F;
    
    public static final int SAFETY_NUMBER_VALUE_0058 = 0x06A3;
    
    public static final int SAFETY_NUMBER_VALUE_0059 = 0x06AC;
    
    public static final int SAFETY_NUMBER_VALUE_0060 = 0x06C5;
    
    public static final int SAFETY_NUMBER_VALUE_0061 = 0x06CA;
    
    public static final int SAFETY_NUMBER_VALUE_0062 = 0x06F6;
    
    public static final int SAFETY_NUMBER_VALUE_0063 = 0x06F9;
    
    public static final int SAFETY_NUMBER_VALUE_0064 = 0x0906;
    
    public static final int SAFETY_NUMBER_VALUE_0065 = 0x0909;
    
    public static final int SAFETY_NUMBER_VALUE_0066 = 0x0935;
    
    public static final int SAFETY_NUMBER_VALUE_0067 = 0x093A;
    
    public static final int SAFETY_NUMBER_VALUE_0068 = 0x0953;
    
    public static final int SAFETY_NUMBER_VALUE_0069 = 0x095C;
    
    public static final int SAFETY_NUMBER_VALUE_0070 = 0x0960;
    
    public static final int SAFETY_NUMBER_VALUE_0071 = 0x096F;
    
    public static final int SAFETY_NUMBER_VALUE_0072 = 0x0990;
    
    public static final int SAFETY_NUMBER_VALUE_0073 = 0x099F;
    
    public static final int SAFETY_NUMBER_VALUE_0074 = 0x09A3;
    
    public static final int SAFETY_NUMBER_VALUE_0075 = 0x09AC;
    
    public static final int SAFETY_NUMBER_VALUE_0076 = 0x09C5;
    
    public static final int SAFETY_NUMBER_VALUE_0077 = 0x09CA;
    
    public static final int SAFETY_NUMBER_VALUE_0078 = 0x09F6;
    
    public static final int SAFETY_NUMBER_VALUE_0079 = 0x09F9;
    
    public static final int SAFETY_NUMBER_VALUE_0080 = 0x0A05;
    
    public static final int SAFETY_NUMBER_VALUE_0081 = 0x0A0A;
    
    public static final int SAFETY_NUMBER_VALUE_0082 = 0x0A36;
    
    public static final int SAFETY_NUMBER_VALUE_0083 = 0x0A39;
    
    public static final int SAFETY_NUMBER_VALUE_0084 = 0x0A50;
    
    public static final int SAFETY_NUMBER_VALUE_0085 = 0x0A5F;
    
    public static final int SAFETY_NUMBER_VALUE_0086 = 0x0A63;
    
    public static final int SAFETY_NUMBER_VALUE_0087 = 0x0A6C;
    
    public static final int SAFETY_NUMBER_VALUE_0088 = 0x0A93;
    
    public static final int SAFETY_NUMBER_VALUE_0089 = 0x0A9C;
    
    public static final int SAFETY_NUMBER_VALUE_0090 = 0x0AA0;
    
    public static final int SAFETY_NUMBER_VALUE_0091 = 0x0AAF;
    
    public static final int SAFETY_NUMBER_VALUE_0092 = 0x0AC6;
    
    public static final int SAFETY_NUMBER_VALUE_0093 = 0x0AC9;
    
    public static final int SAFETY_NUMBER_VALUE_0094 = 0x0AF5;
    
    public static final int SAFETY_NUMBER_VALUE_0095 = 0x0AFA;
    
    public static final int SAFETY_NUMBER_VALUE_0096 = 0x0C03;
    
    public static final int SAFETY_NUMBER_VALUE_0097 = 0x0C0C;
    
    public static final int SAFETY_NUMBER_VALUE_0098 = 0x0C30;
    
    public static final int SAFETY_NUMBER_VALUE_0099 = 0x0C3F;
    
    public static final int SAFETY_NUMBER_VALUE_0100 = 0x0C56;
    
    public static final int SAFETY_NUMBER_VALUE_0101 = 0x0C59;
    
    public static final int SAFETY_NUMBER_VALUE_0102 = 0x0C65;
    
    public static final int SAFETY_NUMBER_VALUE_0103 = 0x0C6A;
    
    public static final int SAFETY_NUMBER_VALUE_0104 = 0x0C95;
    
    public static final int SAFETY_NUMBER_VALUE_0105 = 0x0C9A;
    
    public static final int SAFETY_NUMBER_VALUE_0106 = 0x0CA6;
    
    public static final int SAFETY_NUMBER_VALUE_0107 = 0x0CA9;
    
    public static final int SAFETY_NUMBER_VALUE_0108 = 0x0CC0;
    
    public static final int SAFETY_NUMBER_VALUE_0109 = 0x0CCF;
    
    public static final int SAFETY_NUMBER_VALUE_0110 = 0x0CF3;
    
    public static final int SAFETY_NUMBER_VALUE_0111 = 0x0CFC;
    
    public static final int SAFETY_NUMBER_VALUE_0112 = 0x0F00;
    
    public static final int SAFETY_NUMBER_VALUE_0113 = 0x0F0F;
    
    public static final int SAFETY_NUMBER_VALUE_0114 = 0x0F33;
    
    public static final int SAFETY_NUMBER_VALUE_0115 = 0x0F3C;
    
    public static final int SAFETY_NUMBER_VALUE_0116 = 0x0F55;
    
    public static final int SAFETY_NUMBER_VALUE_0117 = 0x0F5A;
    
    public static final int SAFETY_NUMBER_VALUE_0118 = 0x0F66;
    
    public static final int SAFETY_NUMBER_VALUE_0119 = 0x0F69;
    
    public static final int SAFETY_NUMBER_VALUE_0120 = 0x0F96;
    
    public static final int SAFETY_NUMBER_VALUE_0121 = 0x0F99;
    
    public static final int SAFETY_NUMBER_VALUE_0122 = 0x0FA5;
    
    public static final int SAFETY_NUMBER_VALUE_0123 = 0x0FAA;
    
    public static final int SAFETY_NUMBER_VALUE_0124 = 0x0FC3;
    
    public static final int SAFETY_NUMBER_VALUE_0125 = 0x0FCC;
    
    public static final int SAFETY_NUMBER_VALUE_0126 = 0x0FF0;
    
    public static final int SAFETY_NUMBER_VALUE_0127 = 0x0FFF;
    
    public static final int SAFETY_NUMBER_VALUE_0128 = 0x1111;
    
    public static final int SAFETY_NUMBER_VALUE_0129 = 0x111E;
    
    public static final int SAFETY_NUMBER_VALUE_0130 = 0x1122;
    
    public static final int SAFETY_NUMBER_VALUE_0131 = 0x112D;
    
    public static final int SAFETY_NUMBER_VALUE_0132 = 0x1144;
    
    public static final int SAFETY_NUMBER_VALUE_0133 = 0x114B;
    
    public static final int SAFETY_NUMBER_VALUE_0134 = 0x1177;
    
    public static final int SAFETY_NUMBER_VALUE_0135 = 0x1178;
    
    public static final int SAFETY_NUMBER_VALUE_0136 = 0x1187;
    
    public static final int SAFETY_NUMBER_VALUE_0137 = 0x1188;
    
    public static final int SAFETY_NUMBER_VALUE_0138 = 0x11B4;
    
    public static final int SAFETY_NUMBER_VALUE_0139 = 0x11BB;
    
    public static final int SAFETY_NUMBER_VALUE_0140 = 0x11D2;
    
    public static final int SAFETY_NUMBER_VALUE_0141 = 0x11DD;
    
    public static final int SAFETY_NUMBER_VALUE_0142 = 0x11E1;
    
    public static final int SAFETY_NUMBER_VALUE_0143 = 0x11EE;
    
    public static final int SAFETY_NUMBER_VALUE_0144 = 0x1212;
    
    public static final int SAFETY_NUMBER_VALUE_0145 = 0x121D;
    
    public static final int SAFETY_NUMBER_VALUE_0146 = 0x1221;
    
    public static final int SAFETY_NUMBER_VALUE_0147 = 0x122E;
    
    public static final int SAFETY_NUMBER_VALUE_0148 = 0x1247;
    
    public static final int SAFETY_NUMBER_VALUE_0149 = 0x1248;
    
    public static final int SAFETY_NUMBER_VALUE_0150 = 0x1274;
    
    public static final int SAFETY_NUMBER_VALUE_0151 = 0x127B;
    
    public static final int SAFETY_NUMBER_VALUE_0152 = 0x1284;
    
    public static final int SAFETY_NUMBER_VALUE_0153 = 0x128B;
    
    public static final int SAFETY_NUMBER_VALUE_0154 = 0x12B7;
    
    public static final int SAFETY_NUMBER_VALUE_0155 = 0x12B8;
    
    public static final int SAFETY_NUMBER_VALUE_0156 = 0x12D1;
    
    public static final int SAFETY_NUMBER_VALUE_0157 = 0x12DE;
    
    public static final int SAFETY_NUMBER_VALUE_0158 = 0x12E2;
    
    public static final int SAFETY_NUMBER_VALUE_0159 = 0x12ED;
    
    public static final int SAFETY_NUMBER_VALUE_0160 = 0x1414;
    
    public static final int SAFETY_NUMBER_VALUE_0161 = 0x141B;
    
    public static final int SAFETY_NUMBER_VALUE_0162 = 0x1427;
    
    public static final int SAFETY_NUMBER_VALUE_0163 = 0x1428;
    
    public static final int SAFETY_NUMBER_VALUE_0164 = 0x1441;
    
    public static final int SAFETY_NUMBER_VALUE_0165 = 0x144E;
    
    public static final int SAFETY_NUMBER_VALUE_0166 = 0x1472;
    
    public static final int SAFETY_NUMBER_VALUE_0167 = 0x147D;
    
    public static final int SAFETY_NUMBER_VALUE_0168 = 0x1482;
    
    public static final int SAFETY_NUMBER_VALUE_0169 = 0x148D;
    
    public static final int SAFETY_NUMBER_VALUE_0170 = 0x14B1;
    
    public static final int SAFETY_NUMBER_VALUE_0171 = 0x14BE;
    
    public static final int SAFETY_NUMBER_VALUE_0172 = 0x14D7;
    
    public static final int SAFETY_NUMBER_VALUE_0173 = 0x14D8;
    
    public static final int SAFETY_NUMBER_VALUE_0174 = 0x14E4;
    
    public static final int SAFETY_NUMBER_VALUE_0175 = 0x14EB;
    
    public static final int SAFETY_NUMBER_VALUE_0176 = 0x1717;
    
    public static final int SAFETY_NUMBER_VALUE_0177 = 0x1718;
    
    public static final int SAFETY_NUMBER_VALUE_0178 = 0x1724;
    
    public static final int SAFETY_NUMBER_VALUE_0179 = 0x172B;
    
    public static final int SAFETY_NUMBER_VALUE_0180 = 0x1742;
    
    public static final int SAFETY_NUMBER_VALUE_0181 = 0x174D;
    
    public static final int SAFETY_NUMBER_VALUE_0182 = 0x1771;
    
    public static final int SAFETY_NUMBER_VALUE_0183 = 0x177E;
    
    public static final int SAFETY_NUMBER_VALUE_0184 = 0x1781;
    
    public static final int SAFETY_NUMBER_VALUE_0185 = 0x178E;
    
    public static final int SAFETY_NUMBER_VALUE_0186 = 0x17B2;
    
    public static final int SAFETY_NUMBER_VALUE_0187 = 0x17BD;
    
    public static final int SAFETY_NUMBER_VALUE_0188 = 0x17D4;
    
    public static final int SAFETY_NUMBER_VALUE_0189 = 0x17DB;
    
    public static final int SAFETY_NUMBER_VALUE_0190 = 0x17E7;
    
    public static final int SAFETY_NUMBER_VALUE_0191 = 0x17E8;
    
    public static final int SAFETY_NUMBER_VALUE_0192 = 0x1817;
    
    public static final int SAFETY_NUMBER_VALUE_0193 = 0x1818;
    
    public static final int SAFETY_NUMBER_VALUE_0194 = 0x1824;
    
    public static final int SAFETY_NUMBER_VALUE_0195 = 0x182B;
    
    public static final int SAFETY_NUMBER_VALUE_0196 = 0x1842;
    
    public static final int SAFETY_NUMBER_VALUE_0197 = 0x184D;
    
    public static final int SAFETY_NUMBER_VALUE_0198 = 0x1871;
    
    public static final int SAFETY_NUMBER_VALUE_0199 = 0x187E;
    
    public static final int SAFETY_NUMBER_VALUE_0200 = 0x1881;
    
    public static final int SAFETY_NUMBER_VALUE_0201 = 0x188E;
    
    public static final int SAFETY_NUMBER_VALUE_0202 = 0x18B2;
    
    public static final int SAFETY_NUMBER_VALUE_0203 = 0x18BD;
    
    public static final int SAFETY_NUMBER_VALUE_0204 = 0x18D4;
    
    public static final int SAFETY_NUMBER_VALUE_0205 = 0x18DB;
    
    public static final int SAFETY_NUMBER_VALUE_0206 = 0x18E7;
    
    public static final int SAFETY_NUMBER_VALUE_0207 = 0x18E8;
    
    public static final int SAFETY_NUMBER_VALUE_0208 = 0x1B14;
    
    public static final int SAFETY_NUMBER_VALUE_0209 = 0x1B1B;
    
    public static final int SAFETY_NUMBER_VALUE_0210 = 0x1B27;
    
    public static final int SAFETY_NUMBER_VALUE_0211 = 0x1B28;
    
    public static final int SAFETY_NUMBER_VALUE_0212 = 0x1B41;
    
    public static final int SAFETY_NUMBER_VALUE_0213 = 0x1B4E;
    
    public static final int SAFETY_NUMBER_VALUE_0214 = 0x1B72;
    
    public static final int SAFETY_NUMBER_VALUE_0215 = 0x1B7D;
    
    public static final int SAFETY_NUMBER_VALUE_0216 = 0x1B82;
    
    public static final int SAFETY_NUMBER_VALUE_0217 = 0x1B8D;
    
    public static final int SAFETY_NUMBER_VALUE_0218 = 0x1BB1;
    
    public static final int SAFETY_NUMBER_VALUE_0219 = 0x1BBE;
    
    public static final int SAFETY_NUMBER_VALUE_0220 = 0x1BD7;
    
    public static final int SAFETY_NUMBER_VALUE_0221 = 0x1BD8;
    
    public static final int SAFETY_NUMBER_VALUE_0222 = 0x1BE4;
    
    public static final int SAFETY_NUMBER_VALUE_0223 = 0x1BEB;
    
    public static final int SAFETY_NUMBER_VALUE_0224 = 0x1D12;
    
    public static final int SAFETY_NUMBER_VALUE_0225 = 0x1D1D;
    
    public static final int SAFETY_NUMBER_VALUE_0226 = 0x1D21;
    
    public static final int SAFETY_NUMBER_VALUE_0227 = 0x1D2E;
    
    public static final int SAFETY_NUMBER_VALUE_0228 = 0x1D47;
    
    public static final int SAFETY_NUMBER_VALUE_0229 = 0x1D48;
    
    public static final int SAFETY_NUMBER_VALUE_0230 = 0x1D74;
    
    public static final int SAFETY_NUMBER_VALUE_0231 = 0x1D7B;
    
    public static final int SAFETY_NUMBER_VALUE_0232 = 0x1D84;
    
    public static final int SAFETY_NUMBER_VALUE_0233 = 0x1D8B;
    
    public static final int SAFETY_NUMBER_VALUE_0234 = 0x1DB7;
    
    public static final int SAFETY_NUMBER_VALUE_0235 = 0x1DB8;
    
    public static final int SAFETY_NUMBER_VALUE_0236 = 0x1DD1;
    
    public static final int SAFETY_NUMBER_VALUE_0237 = 0x1DDE;
    
    public static final int SAFETY_NUMBER_VALUE_0238 = 0x1DE2;
    
    public static final int SAFETY_NUMBER_VALUE_0239 = 0x1DED;
    
    public static final int SAFETY_NUMBER_VALUE_0240 = 0x1E11;
    
    public static final int SAFETY_NUMBER_VALUE_0241 = 0x1E1E;
    
    public static final int SAFETY_NUMBER_VALUE_0242 = 0x1E22;
    
    public static final int SAFETY_NUMBER_VALUE_0243 = 0x1E2D;
    
    public static final int SAFETY_NUMBER_VALUE_0244 = 0x1E44;
    
    public static final int SAFETY_NUMBER_VALUE_0245 = 0x1E4B;
    
    public static final int SAFETY_NUMBER_VALUE_0246 = 0x1E77;
    
    public static final int SAFETY_NUMBER_VALUE_0247 = 0x1E78;
    
    public static final int SAFETY_NUMBER_VALUE_0248 = 0x1E87;
    
    public static final int SAFETY_NUMBER_VALUE_0249 = 0x1E88;
    
    public static final int SAFETY_NUMBER_VALUE_0250 = 0x1EB4;
    
    public static final int SAFETY_NUMBER_VALUE_0251 = 0x1EBB;
    
    public static final int SAFETY_NUMBER_VALUE_0252 = 0x1ED2;
    
    public static final int SAFETY_NUMBER_VALUE_0253 = 0x1EDD;
    
    public static final int SAFETY_NUMBER_VALUE_0254 = 0x1EE1;
    
    public static final int SAFETY_NUMBER_VALUE_0255 = 0x1EEE;

    private HammingDistance() 
    { 
        
    }
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
