package com.accu_chek.solo_m.rcapp.application.ble.constant;

import java.util.Arrays;
import java.util.List;

import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;

/**
 */
public final class ControlPointConstant
{
    /**
     * maximum byte length of control point and reponse
     */
    public static final byte MAX_CP_B_LEN = 17;

    /**
	 */
    public static final class TemplateType
    {

        public static final short UNKNOWN = HammingDistance.SAFETY_NUMBER_UINT8_01;
        public static final short BASAL_RATE_PROFILE = HammingDistance.SAFETY_NUMBER_UINT8_02;
        public static final short TBR = HammingDistance.SAFETY_NUMBER_UINT8_03;
        public static final short BOLUS = HammingDistance.SAFETY_NUMBER_UINT8_04;

    }

    /**
     * @author EDLiu
     * @version 1.0
     * @created 11-Feb-2015 6:42:49 PM
     */
    public static final class TBRType
    {

        public static final short UNKNOWN = HammingDistance.SAFETY_NUMBER_UINT8_01;
        public static final short ABSOLUTE = HammingDistance.SAFETY_NUMBER_UINT8_02;
        public static final short RELATIVE = HammingDistance.SAFETY_NUMBER_UINT8_03;

    }

    /**
     * The response codes for the result of requested op code
     * 
     * @author EDLiu
     * @version 1.0
     * @created 11-Feb-2015 6:42:49 PM
     */
    public static final class ResponseCode
    {

        public static final byte SUCCESS = 0x0F;
        public static final byte OP_NOT_SUPPORTED = 0x70;
        public static final byte INVALID_OPERAND = 0x71;
        public static final byte PROC_NOT_COMPLETED = 0x72;
        public static final byte PARAM_OUT_RANGE = 0x73;
        public static final byte PROC_NOT_APPL = 0x74;
        public static final byte PLAUS_FAILED = 0x75;
        public static final byte MAX_BOLUS_NBR_REACHED = 0x76;

    }

    /**
     * @author EDLiu
     * @version 1.0
     * @created 11-Feb-2015 6:42:49 PM
     */
    public static final class OperationState
    {

        public static final short UNKNOWN = HammingDistance.SAFETY_NUMBER_UINT8_01;
        public static final short OFF = HammingDistance.SAFETY_NUMBER_UINT8_02;
        public static final short STANDBY = HammingDistance.SAFETY_NUMBER_UINT8_03;
        public static final short OPERATING = HammingDistance.SAFETY_NUMBER_UINT8_04;

    }

    
    public static final class TherapyControlState
    {

        public static final short UNDETERMINED = HammingDistance.SAFETY_NUMBER_UINT8_01;
        public static final short STOP = HammingDistance.SAFETY_NUMBER_UINT8_02;
        public static final short PAUSE = HammingDistance.SAFETY_NUMBER_UINT8_03;
        public static final short RUN = HammingDistance.SAFETY_NUMBER_UINT8_04;

    }
    
    /**
     * @author EDLiu
     * @version 1.0
     * @created 11-Feb-2015 6:42:49 PM
     */
    public static final class OpCode
    {

        public static final int IDSCMD_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0116;
        public static final int IDSCMD_SET_THERAPY_CONTROL_STATE = HammingDistance.SAFETY_NUMBER_VALUE_0117;
        public static final int IDSCMD_SET_FLIGHT = HammingDistance.SAFETY_NUMBER_VALUE_0118;
        public static final int IDSCMD_SNOOZE_ANNUNCIATION = HammingDistance.SAFETY_NUMBER_VALUE_0119;
        public static final int IDSCMD_SNOOZE_ANNUNCIATION_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0120;
        public static final int IDSCMD_CONFIRM_ANNUNCIATION = HammingDistance.SAFETY_NUMBER_VALUE_0121;
        public static final int IDSCMD_CONFIRM_ANNUNCIATION_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0122;
        public static final int IDSCMD_READ_BASAL_RATE_PROFILE = HammingDistance.SAFETY_NUMBER_VALUE_0123;
        public static final int IDSCMD_READ_BASAL_RATE_PROFILE_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0124;
        public static final int IDSCMD_WRITE_BASAL_RATE_PROFILE = HammingDistance.SAFETY_NUMBER_VALUE_0125;
        public static final int IDSCMD_WRITE_BASAL_RATE_PROFILE_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0126;
        public static final int IDSCMD_SET_TBR_ADJ = HammingDistance.SAFETY_NUMBER_VALUE_0127;
        public static final int IDSCMD_CANCEL_TBR = HammingDistance.SAFETY_NUMBER_VALUE_0128;
        public static final int IDSCMD_GET_TBR_TMPL = HammingDistance.SAFETY_NUMBER_VALUE_0129;
        public static final int IDSCMD_GET_TBR_TMPL_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0130;
        public static final int IDSCMD_SET_TBR_TMPL = HammingDistance.SAFETY_NUMBER_VALUE_0131;
        public static final int IDSCMD_SET_TBR_TMPL_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0132;
        public static final int IDSCMD_SET_BOLUS = HammingDistance.SAFETY_NUMBER_VALUE_0133;
        public static final int IDSCMD_SET_BOLUS_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0134;
        public static final int IDSCMD_CANCEL_BOLUS = HammingDistance.SAFETY_NUMBER_VALUE_0135;
        public static final int IDSCMD_CANCEL_BOLUS_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0136;
        public static final int IDSCMD_GET_AVAIL_BOLUS = HammingDistance.SAFETY_NUMBER_VALUE_0137;
        public static final int IDSCMD_GET_AVAIL_BOLUS_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0138;
        public static final int IDSCMD_GET_TMPL_STATUS = HammingDistance.SAFETY_NUMBER_VALUE_0143;
        public static final int IDSCMD_GET_TMPL_STATUS_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0144;
        public static final int IDSCMD_RESET_TMPL_STATUS = HammingDistance.SAFETY_NUMBER_VALUE_0145;
        public static final int IDSCMD_RESET_TMPL_STATUS_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0146;
        public static final int IDSCMD_ACT_PROFILE_TMPL = HammingDistance.SAFETY_NUMBER_VALUE_0147;
        public static final int IDSCMD_ACT_PROFILE_TMPL_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0148;
        public static final int IDSCMD_GET_PROFILE_TMPL = HammingDistance.SAFETY_NUMBER_VALUE_0149;
        public static final int IDSCMD_GET_PROFILE_TMPL_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0150;
        public static final int IDSCMD_START_PRIMING = HammingDistance.SAFETY_NUMBER_VALUE_0151;
        public static final int IDSCMD_STOP_PRIMING = HammingDistance.SAFETY_NUMBER_VALUE_0152;
        public static final int IDSCMD_INIT_RSV_LVL = HammingDistance.SAFETY_NUMBER_VALUE_0153;
        public static final int IDSCMD_RESET_RSV_OP_TIME = HammingDistance.SAFETY_NUMBER_VALUE_0154;

        public static final int IDSREAD_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0016;
        public static final int IDSREAD_RESET_STATUS = HammingDistance.SAFETY_NUMBER_VALUE_0017;
        public static final int IDSREAD_ACTIVE_BOLUS_ID = HammingDistance.SAFETY_NUMBER_VALUE_0018;
        public static final int IDSREAD_ACTIVE_BOLUS_ID_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0019;
        public static final int IDSREAD_ACTIVE_BOLUS = HammingDistance.SAFETY_NUMBER_VALUE_0020;
        public static final int IDSREAD_ACTIVE_BOLUS_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0021;
        public static final int IDSREAD_ACTIVE_BASAL = HammingDistance.SAFETY_NUMBER_VALUE_0022;
        public static final int IDSREAD_ACTIVE_BASAL_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0023;
        public static final int IDSREAD_TOTAL_DAILY_INSULIN = HammingDistance.SAFETY_NUMBER_VALUE_0024;
        public static final int IDSREAD_TOTAL_DAILY_INSULIN_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0025;
        public static final int IDSREAD_DATE_TIME = HammingDistance.SAFETY_NUMBER_VALUE_0026;
        public static final int IDSREAD_DATE_TIME_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0027;
        public static final int IDSREAD_COUNTER = HammingDistance.SAFETY_NUMBER_VALUE_0028;
        public static final int IDSREAD_COUNTER_RESP = HammingDistance.SAFETY_NUMBER_VALUE_0029;

        public static final int SOLOM_SERVICE_IND_RESP = 0x0001;

        public static final int SOLOM_CP_RESP = 0x2112;
        public static final int SOLOM_RESET_STATUS = 0x2148;
        public static final int SOLOM_SET_CONFIG_BLOCK1 = 0x2174;
        public static final int SOLOM_SET_CONFIG_BLOCK2 = 0x217B;
        public static final int SOLOM_READ_CONFIG_BLOCK1 = 0x2184;
        public static final int SOLOM_READ_CONFIG_BLOCK1_RESP = 0x218B;
        public static final int SOLOM_READ_CONFIG_BLOCK2 = 0x21B7;
        public static final int SOLOM_READ_CONFIG_BLOCK2_RESP = 0x21B8;
        public static final int SOLOM_SEND_AUTO_OFF_TIMESTAMP = 0x21ED;
        public static final int SOLOM_GET_DATE_AND_TIME = 0x224B;
        public static final int SOLOM_GET_DATE_AND_TIME_RESP = 0x2277;
        public static final int SOLOM_SET_DATE_AND_TIME = 0x2278;

        public static final int KES_VERSION_REQUEST = 0x0101;
//        public static final int KES_VERSION_REQUEST_RESPONSE = 0x0201;
        public static final int KES_RESPONSE_CODE = 0x0003;
        public static final int KES_ABORT_PROCEDURE = 0x000F;
        public static final int KES_STEP1 = 0x0010;
        public static final int KES_SETP2 = 0x0011;
        public static final int KES_SETP3 = 0x0012;

        // public static final int KES_M1_DATA0 = 0x2000;
        // public static final int KES_M1_DATA1 = 0x2010;
        // public static final int KES_M1_DATA2 = 0x2020;
        // public static final int KES_M1_DATA3 = 0x2030;
        // public static final int KES_M1_DATA4 = 0x2040;
        // public static final int KES_M1_DATA5 = 0x2050;
        // public static final int KES_M1_DATA6 = 0x2060;
        // public static final int KES_M1_DATA7 = 0x2070;
        // public static final int KES_M1_DATA8 = 0x2080;
        // public static final int KES_M1_DATA9 = 0x2090;
        // public static final int KES_M1_DATA10 = 0x20A0;
        // public static final int KES_M1_DATA11 = 0x20B0;
        // public static final int KES_M1_DATA12 = 0x20C0;
        // public static final int KES_M1_DATA13 = 0x20D0;
        // public static final int KES_M1_DATA14 = 0x20E0;
        // public static final int KES_M1_DATA15 = 0x20F0;
        //
        // public static final int KES_M3_DATA0 = 0x2200;
        // public static final int KES_M3_DATA1 = 0x2210;
        // public static final int KES_M3_DATA2 = 0x2220;
        // public static final int KES_M3_DATA3 = 0x2230;
        // public static final int KES_M3_DATA4 = 0x2240;
        // public static final int KES_M3_DATA5 = 0x2250;
        // public static final int KES_M3_DATA6 = 0x2260;
        // public static final int KES_M3_DATA7 = 0x2270;
        // public static final int KES_M3_DATA8 = 0x2280;
        // public static final int KES_M3_DATA9 = 0x2290;
        // public static final int KES_M3_DATA10 = 0x22A0;
        // public static final int KES_M3_DATA11 = 0x22B0;
        // public static final int KES_M3_DATA12 = 0x22C0;
        // public static final int KES_M3_DATA13 = 0x22D0;
        // public static final int KES_M3_DATA14 = 0x22E0;
        // public static final int KES_M3_DATA15 = 0x22F0;
        //
        // public static final int KES_M5_DATA0 = 0x3100;
        // public static final int KES_M5_DATA1 = 0x3110;

        public static final int KES_VERSION_REQUEST_RESPONSE = 0x02;
        public static final int KES_M1_DATA = 0x20;
        public static final int KES_M2_DATA = 0x2100;
        public static final int KES_M3_DATA = 0x22;
        public static final int KES_M4_DATA = 0x3000;
        public static final int KES_M5_DATA = 0x31;
        public static final int KES_M6_DATA = 0x3200;

    }

    /**
     * @author EDLiu
     * @version 1.0
     * @created 11-Feb-2015 6:42:50 PM
     */
    public static final class KESOpCode
    {
        public static final byte VERSION_REQUEST = 0x01;
        public static final byte VERSION_REQUEST_RESPONSE = 0x02;
        public static final byte RESPONSE_CODE = 0x03;
        public static final byte ABORT_PROCEDURE = 0x0F;
        public static final byte STEP1 = 0x10;
        public static final byte STEP2 = 0x11;
        public static final byte STEP3 = 0x12;
        public static final byte M1_DATA = 0x20;
        public static final byte M2_DATA = 0x21;
        public static final byte M3_DATA = 0x22;
        public static final byte M4_DATA = 0x30;
        public static final byte M5_DATA = 0x31;
        public static final byte M6_DATA = 0x32;
    }

    /**
     * @author EDLiu
     * @version 1.0
     * @created 11-Feb-2015 6:42:50 PM
     */
    public static final class DeviceStatus
    {

        public static final short UNKNOWN = HammingDistance.SAFETY_NUMBER_UINT8_01;
        public static final short REWINDING = HammingDistance.SAFETY_NUMBER_UINT8_02;
        public static final short SNIFFING = HammingDistance.SAFETY_NUMBER_UINT8_03;
        public static final short PRIMING = HammingDistance.SAFETY_NUMBER_UINT8_04;
        public static final short WAITING = HammingDistance.SAFETY_NUMBER_UINT8_05;
        public static final short READY = HammingDistance.SAFETY_NUMBER_UINT8_06;

    }

    
    /**
     * @author EDLiu
     * @version 1.0
     * @created 11-Feb-2015 6:42:50 PM
     */
    public static final class CounterType
    {

        public static final short LIEFTIME = HammingDistance.SAFETY_NUMBER_UINT8_01;
        public static final short WARRANTY = HammingDistance.SAFETY_NUMBER_UINT8_02;
        public static final short LOANER = HammingDistance.SAFETY_NUMBER_UINT8_03;
        public static final short RSVR_OP_TIME = HammingDistance.SAFETY_NUMBER_UINT8_04;

    }

    /**
     * @author EDLiu
     * @version 1.0
     * @created 11-Feb-2015 6:42:50 PM
     */
    public static final class BolusValueSelection
    {

        public static final short PROGRAMMED = HammingDistance.SAFETY_NUMBER_UINT8_01;
        public static final short REMAINING = HammingDistance.SAFETY_NUMBER_UINT8_02;

    }

    /**
     * @author EDLiu
     * @version 1.0
     * @created 11-Feb-2015 6:42:51 PM
     */
    public static final class BolusType
    {

        public static final short FAST = HammingDistance.SAFETY_NUMBER_UINT8_01;
        public static final short EXTENDED = HammingDistance.SAFETY_NUMBER_UINT8_02;
        public static final short MULTIWAVE = HammingDistance.SAFETY_NUMBER_UINT8_03;

    }

    /**
     * @author EDLiu
     * @version 1.0
     * @created 11-Feb-2015 6:42:51 PM
     */
    public static final class BolusActivationType
    {

        public static final short UNKNOWN = HammingDistance.SAFETY_NUMBER_UINT8_01;
        public static final short MANUAL = HammingDistance.SAFETY_NUMBER_UINT8_02;
        public static final short RECOMMENDED = HammingDistance.SAFETY_NUMBER_UINT8_03;
        public static final short MANUAL_RECOMMENDED = HammingDistance.SAFETY_NUMBER_UINT8_04;
        public static final short COMMANDED = HammingDistance.SAFETY_NUMBER_UINT8_05;

    }

    /**
     * @author EDLiu
     * @version 1.0
     * @created 11-Feb-2015 6:42:51 PM
     */
    public static final class BatterySymbol
    {

        public static final short UNKNOWN = HammingDistance.SAFETY_NUMBER_UINT8_01;
        public static final short FULL = HammingDistance.SAFETY_NUMBER_UINT8_02;
        public static final short MEDIUM = HammingDistance.SAFETY_NUMBER_UINT8_03;
        public static final short LOW = HammingDistance.SAFETY_NUMBER_UINT8_04;
        public static final short EMPTY = HammingDistance.SAFETY_NUMBER_UINT8_05;

    }

    public static final class OpGroup
    {
        public static final List<Integer> IDS_READ = Arrays.asList();

        public static final List<Integer> IDS_CMD = Arrays.asList();

        public static final List<Integer> SOLOM = Arrays.asList();

        public static final List<Integer> KEY_EXCHANGE = Arrays.asList();
        
        public static final List<Integer> SOLOM_HISTORY = Arrays.asList();

    }

}
// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
