package com.accu_chek.solo_m.rcapp.application.ble.constant;

import java.util.Arrays;
import java.util.List;

import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;

/**
 * @author EDLiu
 * @version 1.0
 * @created 27-Jan-2015 1:37:04 PM
 */
public final class CommsConstant
{
    /**
     * LL flag
     */
    public static final byte LLF = 0x7E; // LL flag
    
    /**
     * Length of LLF byte
     */
    public static final byte LLF_BYTE_LEN = 1;
    
    /**
     * Length of the length byte
     */
    public static final byte LEN_BYTE_LEN = 1;
    
    /**
     * Length of the sequence byte
     */
    public static final byte SEQ_BYTE_LEN = 1;
    
    /**
     * Length of the CRC bytes
     */
    public static final byte CRC_BYTE_LEN = 2;
    
    /**
     * Length of the group id bytes
     */
    public static final byte GROUP_BYTE_LEN = 2;
    
    /**
     * Length of the code id bytes
     */
    public static final byte CODE_BYTE_LEN = 2;
    
    /**
     * Maximum length of frame bytes
     */
    public static final byte MAX_FRM_LEN = 118;
    
    /**
     * Maximum length of payload bytes
     */
    public static final byte MAX_PL_LEN = 112;
    
    /**
     * Maximum length of message bytes
     */
    public static final byte MAX_MSG_LEN = 108;
    
    /**
     * Length of a SFLOAT
     */
    public static final byte SFLOAT_BYTE_LEN = 2;
    
    /**
     * The NAN represent this value is not a number.
     */
    public static final short NAN = 0x07FF;
    
    /**
     * Used to convert integer from signed to unsigned.
     */
    public static final short TO_POSITIVE = 0xFF;
    

    /**
     * 
     * Unit: Hamming Distance
     */
    public static final class BtState
    {

        public static final short IDLE = HammingDistance.SAFETY_NUMBER_UINT8_01;
        public static final short CONNECTIONLOST= HammingDistance.SAFETY_NUMBER_UINT8_02;
        public static final short DISCONNECTED = HammingDistance.SAFETY_NUMBER_UINT8_03;
        public static final short CONNECTED = HammingDistance.SAFETY_NUMBER_UINT8_04;
        public static final short SYNC_STATUS = HammingDistance.SAFETY_NUMBER_UINT8_05;
        public static final short RADIO_OFF = HammingDistance.SAFETY_NUMBER_UINT8_06;

    }

    
    /**
     * 
     * Unit: Hamming Distance
     */
    public static final class UiState
    {

        public static final short ACTIVE = HammingDistance.SAFETY_NUMBER_UINT8_01;
        public static final short STANDBY = HammingDistance.SAFETY_NUMBER_UINT8_02;

    }

    
    /**
     * 
     * Unit: Hamming Distance
     */
    public static final class Result
    {

        public static final int RESULT_OK = HammingDistance.SAFETY_NUMBER_VALUE_0001;
        public static final int INVALID_CRC = HammingDistance.SAFETY_NUMBER_VALUE_0002;
        public static final int INVALID_SEQ = HammingDistance.SAFETY_NUMBER_VALUE_0003;
        public static final int BT_ERROR = HammingDistance.SAFETY_NUMBER_VALUE_0004;
        public static final int ONGOING = HammingDistance.SAFETY_NUMBER_VALUE_0005;
        public static final int INVALID_PARAM = HammingDistance.SAFETY_NUMBER_VALUE_0006;
        public static final int INVALID_PARAM_NUM = HammingDistance.SAFETY_NUMBER_VALUE_0007;
        public static final int INVALID_CMD = HammingDistance.SAFETY_NUMBER_VALUE_0008;
        public static final int INVALID_GROUP = HammingDistance.SAFETY_NUMBER_VALUE_0009;
        public static final int INVALID_TUNNEL = HammingDistance.SAFETY_NUMBER_VALUE_0010;
        public static final int BUSY = HammingDistance.SAFETY_NUMBER_VALUE_0011;
        public static final int NO_RETURN = HammingDistance.SAFETY_NUMBER_VALUE_0012;
        public static final int DATA_ERROR = HammingDistance.SAFETY_NUMBER_VALUE_0013;

    }

    /**
     * E2E result in BT_ATTR_NOTIF_IND
     * Unit: Hamming Distance
     */
    public static final class E2E_Result
    {

        public static final int RESULT_OK = HammingDistance.SAFETY_NUMBER_UINT8_01;
        public static final int CRC_ERROR = HammingDistance.SAFETY_NUMBER_UINT8_02;
        public static final int COUNTER_ERROR = HammingDistance.SAFETY_NUMBER_UINT8_03;
        public static final int UNKNOWN = HammingDistance.SAFETY_NUMBER_UINT8_04;
    }
    
    
    
    /**
     * The commands are divided into groups.
     * 
     * Unit: Hamming Distance
     */
    public static final class GroupId
    {

        /**
         * "COMM_PROCESSOR" is for commands about comms local message.
         */
        public static final int COMM_PROCESSOR = HammingDistance.SAFETY_NUMBER_UINT16_0001;
        public static final int TUNNEL_REQUEST = HammingDistance.SAFETY_NUMBER_UINT16_0002;
        public static final int NVRAM_TUNNEL = HammingDistance.SAFETY_NUMBER_UINT16_0003;
        public static final int SERVICE_RECORD = HammingDistance.SAFETY_NUMBER_UINT16_0004;
        public static final int BTLE = HammingDistance.SAFETY_NUMBER_UINT16_0005;
        public static final int TEST_MODE = HammingDistance.SAFETY_NUMBER_UINT16_0006;
        public static final int FLASH_TUNNEL = HammingDistance.SAFETY_NUMBER_UINT16_0007;
        public static final int ACK = HammingDistance.SAFETY_NUMBER_UINT16_0015;

    }

    
    /**
     * Record Type for commands of group "Service Record".
     * 
     * Unit: Hamming Distance
     * 
     */
    public static final class RecordType
    {
        public static final int IDS = HammingDistance.SAFETY_NUMBER_UINT8_01;
        public static final int ACTIVE_BOLUS = HammingDistance.SAFETY_NUMBER_UINT8_02;
        public static final int HISTORY = HammingDistance.SAFETY_NUMBER_UINT8_03;

    }

    
    /**
     * 
     * 
     * Unit: Hamming Distance
     */
    public static final class CommandCode
    {
        // new comms CMD ID - begin
        // Comms Processor code
        public static final int COMM_NO_CODE = HammingDistance.SAFETY_NUMBER_VALUE_0001;
        public static final int COMM_READY_IND = HammingDistance.SAFETY_NUMBER_VALUE_0002;
        public static final int COMM_CONFIG_DATA = HammingDistance.SAFETY_NUMBER_VALUE_0003; 
        public static final int COMM_GET_INFO = HammingDistance.SAFETY_NUMBER_VALUE_0004; 
        public static final int COMM_DATA_SENT = HammingDistance.SAFETY_NUMBER_VALUE_0005;
        public static final int COMM_SET_FLIGHT_MODE = HammingDistance.SAFETY_NUMBER_VALUE_0006; 
                                                                                                 
        public static final int COMM_BG_MODE = HammingDistance.SAFETY_NUMBER_VALUE_0007;
        public static final int COMM_PRE_POWER_OFF = HammingDistance.SAFETY_NUMBER_VALUE_0008;
        public static final int COMM_UI_STATE_UPDATE = HammingDistance.SAFETY_NUMBER_VALUE_0009; 
        public static final int COMM_BT_RMV_BOND = HammingDistance.SAFETY_NUMBER_VALUE_0010; 
        public static final int COMM_FLIGHT_MODE_STATUS = HammingDistance.SAFETY_NUMBER_VALUE_0011; 

        // Configuration data request
        public static final int CONFIG_REQ_IND = HammingDistance.SAFETY_NUMBER_VALUE_0012;
        public static final int CONFIG_REQ_CNF = HammingDistance.SAFETY_NUMBER_VALUE_0013; 
        public static final int COMM_GET_ERROR_LOG = HammingDistance.SAFETY_NUMBER_VALUE_0014; 
        // Runtime test
        public static final int RUNTIME_TEST = HammingDistance.SAFETY_NUMBER_VALUE_0020; 
        public static final int MFTT_TEST = HammingDistance.SAFETY_NUMBER_VALUE_0021; 
        // Software Watchdog commands
        public static final int WATCHDOG_CHAL_REQ = HammingDistance.SAFETY_NUMBER_VALUE_0022;
        public static final int WATCHDOG_CHAL_IND = HammingDistance.SAFETY_NUMBER_VALUE_0023;
        public static final int WATCHDOG_CHAL_CFM = HammingDistance.SAFETY_NUMBER_VALUE_0024;
        public static final int WATCHDOG_SUSPEND = HammingDistance.SAFETY_NUMBER_VALUE_0025;
        // BT Service codes
        public static final int BT_UNKNOWN = HammingDistance.SAFETY_NUMBER_VALUE_0030; 
        public static final int BT_INTERNAL_EVENT = HammingDistance.SAFETY_NUMBER_VALUE_0031; 
        public static final int BT_DISCOVERY_SRV = HammingDistance.SAFETY_NUMBER_VALUE_0032; 
        public static final int BT_DISCOVERY_INFO = HammingDistance.SAFETY_NUMBER_VALUE_0033; 
        public static final int BT_SCAN = HammingDistance.SAFETY_NUMBER_VALUE_0034; 
        public static final int BT_SCAN_INFO = HammingDistance.SAFETY_NUMBER_VALUE_0035; 
        public static final int BT_CONNECT = HammingDistance.SAFETY_NUMBER_VALUE_0036; 
        public static final int BT_DISCONNECT = HammingDistance.SAFETY_NUMBER_VALUE_0037; 
        public static final int BT_CONN_UPDATE = HammingDistance.SAFETY_NUMBER_VALUE_0038; 
        public static final int BT_PAIRABLE = HammingDistance.SAFETY_NUMBER_VALUE_0039; 
        public static final int BT_SECURITY = HammingDistance.SAFETY_NUMBER_VALUE_0040; 
        public static final int BT_OOB_IND = HammingDistance.SAFETY_NUMBER_VALUE_0041; 
        public static final int BT_OOB_CFM = HammingDistance.SAFETY_NUMBER_VALUE_0042; 
        public static final int BT_ATTR_READ = HammingDistance.SAFETY_NUMBER_VALUE_0043;
        public static final int BT_ATTR_WRITE = HammingDistance.SAFETY_NUMBER_VALUE_0044;
        public static final int BT_ATTR_NOTIF_IND = HammingDistance.SAFETY_NUMBER_VALUE_0045;
        // BT functions
        public static final int BT_CCCD_CONFIG = HammingDistance.SAFETY_NUMBER_VALUE_0050; 
        public static final int BT_SYNC_STATUS_START = HammingDistance.SAFETY_NUMBER_VALUE_0051; 
        public static final int BT_SYNC_STATUS_END = HammingDistance.SAFETY_NUMBER_VALUE_0052; 
        public static final int BT_ABORT_HIST_UPDATE = HammingDistance.SAFETY_NUMBER_VALUE_0053; 
        public static final int BT_GET_PUMP_HIST = HammingDistance.SAFETY_NUMBER_VALUE_0054; 
        public static final int BT_SYNC_DEVICE_STATUS_IND = HammingDistance.SAFETY_NUMBER_VALUE_0055; 
        public static final int BT_SYNC_DEVICE_STATUS_CFM = HammingDistance.SAFETY_NUMBER_VALUE_0056; 
        public static final int BT_SOLOM_EMWR_IND = HammingDistance.SAFETY_NUMBER_VALUE_0057; 
        public static final int BT_SOLOM_EMWR_CFM = HammingDistance.SAFETY_NUMBER_VALUE_0058; 
        public static final int BT_SOLOM_CONFIG_IND = HammingDistance.SAFETY_NUMBER_VALUE_0059; 
        public static final int BT_SOLOM_CONFIG_CFM = HammingDistance.SAFETY_NUMBER_VALUE_0060; 
        public static final int BT_TIME_SYNC_IND = HammingDistance.SAFETY_NUMBER_VALUE_0061; 
        public static final int BT_TIME_SYNC_CFM = HammingDistance.SAFETY_NUMBER_VALUE_0062; 
        public static final int BT_SYNC_OPERATION_IND = HammingDistance.SAFETY_NUMBER_VALUE_0063;
        public static final int BT_SYNC_OPERATION_CFM = HammingDistance.SAFETY_NUMBER_VALUE_0064;
        public static final int BT_RESET_CONNECTION = HammingDistance.SAFETY_NUMBER_VALUE_0065;
        public static final int BT_CONNECTION_STATE = HammingDistance.SAFETY_NUMBER_VALUE_0066;
        public static final int BT_NO_CONNECTION_IND = HammingDistance.SAFETY_NUMBER_VALUE_0067;
        public static final int BT_NO_CONNECTION_CFM = HammingDistance.SAFETY_NUMBER_VALUE_0068;
        public static final int BT_STATUS_UPDATE_IND = HammingDistance.SAFETY_NUMBER_VALUE_0069;
        public static final int BT_STATUS_UPDATE_CFM = HammingDistance.SAFETY_NUMBER_VALUE_0070;
        public static final int BT_DEFECT_ALERT = HammingDistance.SAFETY_NUMBER_VALUE_0071;
        public static final int BT_SYNC_TOTAL_INSULIN_IND= HammingDistance.SAFETY_NUMBER_VALUE_0072; 
        public static final int BT_SYNC_TOTAL_INSULIN_CFM= HammingDistance.SAFETY_NUMBER_VALUE_0073; 
        public static final int BT_BASAL_RATE_IND= HammingDistance.SAFETY_NUMBER_VALUE_0074;    
        public static final int BT_BASAL_RATE_CFM= HammingDistance.SAFETY_NUMBER_VALUE_0075;    
        public static final int BT_ACTIVE_BOLUS_IND= HammingDistance.SAFETY_NUMBER_VALUE_0076;  
        public static final int BT_ACTIVE_BOLUS_CFM= HammingDistance.SAFETY_NUMBER_VALUE_0077;  
        public static final int BT_SET_TIME_STAMP = HammingDistance.SAFETY_NUMBER_VALUE_0078;   
        public static final int BT_SYSTEM_SYNC = HammingDistance.SAFETY_NUMBER_VALUE_0079;
        // Tunnel Access codes
        public static final int TUNNEL_CLOSE = HammingDistance.SAFETY_NUMBER_VALUE_0080;
        public static final int TUNNEL_OPEN = HammingDistance.SAFETY_NUMBER_VALUE_0081;
        // Pump commands
        public static final int PUMP_REC_READ = HammingDistance.SAFETY_NUMBER_VALUE_0082; 
        public static final int PUMP_REC_ERASE = HammingDistance.SAFETY_NUMBER_VALUE_0083; 
        public static final int PUMP_REC_SEQ_READ = HammingDistance.SAFETY_NUMBER_VALUE_0084; 
        public static final int PUMP_REC_ERASE_SOLOM = HammingDistance.SAFETY_NUMBER_VALUE_0085; 
        public static final int PUMP_REC_FULL_SOLOM = HammingDistance.SAFETY_NUMBER_VALUE_0086; 
        public static final int PUMP_MORE_DATA = HammingDistance.SAFETY_NUMBER_VALUE_0087; 
        public static final int PUMP_NO_MORE_DATA = HammingDistance.SAFETY_NUMBER_VALUE_0088; 
        // public static final int PUMP_NV_READ =
        // HammingDistance.SAFETY_NUMBER_VALUE_0080; //NV_READ
        // public static final int PUMP_NV_ERASE =
        // HammingDistance.SAFETY_NUMBER_VALUE_0081; //NV_ERASE
        // For Test only
        public static final int COMM_LINK_STATUS = HammingDistance.SAFETY_NUMBER_VALUE_0102;
        public static final int COMM_LINK_STATUS_IND = HammingDistance.SAFETY_NUMBER_VALUE_0103;
        public static final int COMM_CLEAR_MEMORY = HammingDistance.SAFETY_NUMBER_VALUE_0107;

        // new CMD ID - end

    }

    /**
     * Group id is the first byte of "payload" byte array and command code is
     * the byte next to it. Commands are divided into groups in Comms subsystem.
     * 
     * 
     */

    public final static class CommandGroup
    {
        /**
         * command code list of group Comms Processor
         * 
         * Unit: Hamming Distance
         */

        public static final List<Integer> COMMS = Arrays.asList(

                CommsConstant.CommandCode.COMM_NO_CODE,
                CommsConstant.CommandCode.COMM_READY_IND,
                CommsConstant.CommandCode.COMM_CONFIG_DATA,
                CommsConstant.CommandCode.COMM_GET_INFO,
                CommsConstant.CommandCode.COMM_DATA_SENT,
                CommsConstant.CommandCode.COMM_SET_FLIGHT_MODE,
                CommsConstant.CommandCode.COMM_BG_MODE,
                CommsConstant.CommandCode.COMM_PRE_POWER_OFF,
                CommsConstant.CommandCode.COMM_UI_STATE_UPDATE,
                CommsConstant.CommandCode.COMM_BT_RMV_BOND,
                CommsConstant.CommandCode.COMM_FLIGHT_MODE_STATUS,
                CommsConstant.CommandCode.COMM_GET_ERROR_LOG,
                CommsConstant.CommandCode.CONFIG_REQ_IND,
                CommsConstant.CommandCode.CONFIG_REQ_CNF,
                CommsConstant.CommandCode.RUNTIME_TEST,             
                CommsConstant.CommandCode.MFTT_TEST,
                CommsConstant.CommandCode.WATCHDOG_CHAL_REQ,
                CommsConstant.CommandCode.WATCHDOG_CHAL_IND,
                CommsConstant.CommandCode.WATCHDOG_CHAL_CFM,
                CommsConstant.CommandCode.WATCHDOG_SUSPEND,
                CommsConstant.CommandCode.COMM_CLEAR_MEMORY);
        

        /**
         * command code list of BTLE
         * 
         * Unit: Hamming Distance
         */
        public static final List<Integer> BTLE = Arrays.asList(
                CommsConstant.CommandCode.BT_UNKNOWN,
                CommsConstant.CommandCode.BT_INTERNAL_EVENT,
                CommsConstant.CommandCode.BT_DISCOVERY_SRV,
                CommsConstant.CommandCode.BT_DISCOVERY_INFO,
                CommsConstant.CommandCode.BT_SCAN,
                CommsConstant.CommandCode.BT_SCAN_INFO,
                CommsConstant.CommandCode.BT_CONNECT,
                CommsConstant.CommandCode.BT_DISCONNECT,
                CommsConstant.CommandCode.BT_CONN_UPDATE,
                CommsConstant.CommandCode.BT_PAIRABLE,
                CommsConstant.CommandCode.BT_SECURITY,
                CommsConstant.CommandCode.BT_OOB_IND,
                CommsConstant.CommandCode.BT_OOB_CFM,
                CommsConstant.CommandCode.BT_ATTR_READ,
                CommsConstant.CommandCode.BT_ATTR_WRITE,
                CommsConstant.CommandCode.BT_ATTR_NOTIF_IND,
                CommsConstant.CommandCode.BT_CCCD_CONFIG,
                CommsConstant.CommandCode.BT_SYNC_STATUS_START,
                CommsConstant.CommandCode.BT_SYNC_STATUS_END,
                CommsConstant.CommandCode.BT_ABORT_HIST_UPDATE,
                CommsConstant.CommandCode.BT_GET_PUMP_HIST,
                CommsConstant.CommandCode.BT_SYNC_DEVICE_STATUS_IND,
                CommsConstant.CommandCode.BT_SYNC_DEVICE_STATUS_CFM,
                CommsConstant.CommandCode.BT_SOLOM_EMWR_IND,
                CommsConstant.CommandCode.BT_SOLOM_EMWR_CFM,
                CommsConstant.CommandCode.BT_SOLOM_CONFIG_IND,
                CommsConstant.CommandCode.BT_SOLOM_CONFIG_CFM,
                CommsConstant.CommandCode.BT_TIME_SYNC_IND,
                CommsConstant.CommandCode.BT_TIME_SYNC_CFM,
                CommsConstant.CommandCode.BT_SYNC_OPERATION_IND,
                CommsConstant.CommandCode.BT_SYNC_OPERATION_CFM,
                CommsConstant.CommandCode.BT_RESET_CONNECTION,
                CommsConstant.CommandCode.BT_CONNECTION_STATE,
                CommsConstant.CommandCode.BT_NO_CONNECTION_IND,
                CommsConstant.CommandCode.BT_NO_CONNECTION_CFM,
                CommsConstant.CommandCode.BT_STATUS_UPDATE_IND,
                CommsConstant.CommandCode.BT_SYNC_TOTAL_INSULIN_IND,
                CommsConstant.CommandCode.BT_SYNC_TOTAL_INSULIN_CFM,
                CommsConstant.CommandCode.BT_BASAL_RATE_IND,
                CommsConstant.CommandCode.BT_BASAL_RATE_CFM,
                CommsConstant.CommandCode.BT_ACTIVE_BOLUS_IND,
                CommsConstant.CommandCode.BT_ACTIVE_BOLUS_CFM,
                CommsConstant.CommandCode.BT_SET_TIME_STAMP,
                CommsConstant.CommandCode.COMM_LINK_STATUS,
                CommsConstant.CommandCode.COMM_LINK_STATUS_IND,
                CommsConstant.CommandCode.COMM_LINK_STATUS_IND,
                CommsConstant.CommandCode.BT_SYSTEM_SYNC);
        
        
        /**
         * command code list of Tunnel Access
         */
        public static final List<Integer> TUNNEL = Arrays.asList(
                CommsConstant.CommandCode.TUNNEL_CLOSE,
                CommsConstant.CommandCode.TUNNEL_OPEN);
        
        
        /**
         * command code list of Record Access (Pump Data included.)
         * 
         * Unit: Hamming Distance
         */
        public static final List<Integer> RECORD = Arrays.asList(
                CommsConstant.CommandCode.PUMP_REC_READ,
                CommsConstant.CommandCode.PUMP_REC_ERASE,
                CommsConstant.CommandCode.PUMP_REC_SEQ_READ,
                CommsConstant.CommandCode.PUMP_REC_ERASE_SOLOM,
                CommsConstant.CommandCode.PUMP_REC_FULL_SOLOM,
                CommsConstant.CommandCode.PUMP_MORE_DATA,
                CommsConstant.CommandCode.PUMP_NO_MORE_DATA);
        public static final List<Integer> NVRAM = Arrays.asList();

        /**
         * command code list of No Token
         */
        public static final List<Integer> NO_TOKEN = Arrays.asList(
                CommsConstant.CommandCode.BT_SYNC_DEVICE_STATUS_CFM,
                CommsConstant.CommandCode.BT_SOLOM_EMWR_CFM,
                CommsConstant.CommandCode.BT_SOLOM_CONFIG_CFM,
                CommsConstant.CommandCode.BT_TIME_SYNC_CFM,
                CommsConstant.CommandCode.BT_SYNC_TOTAL_INSULIN_CFM,
                CommsConstant.CommandCode.BT_ACTIVE_BOLUS_CFM,
                CommsConstant.CommandCode.BT_BASAL_RATE_CFM,
                CommsConstant.CommandCode.BT_SYSTEM_SYNC);

    }

}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
