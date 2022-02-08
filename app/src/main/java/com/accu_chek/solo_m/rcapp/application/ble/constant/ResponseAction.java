package com.accu_chek.solo_m.rcapp.application.ble.constant;

import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeChangeNotification;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeReadResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeWriteResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.BTInternalEventNotification;
import com.accu_chek.solo_m.rcapp.application.ble.response.BlankMessageResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.CCCDResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.CauseOnlyResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.CommandDataSentIndication;
import com.accu_chek.solo_m.rcapp.application.ble.response.CommsInfoResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.CommsReadyIndication;
import com.accu_chek.solo_m.rcapp.application.ble.response.ConfigurationDataResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.ConnectResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.ConnectionStateIndication;
import com.accu_chek.solo_m.rcapp.application.ble.response.ErrorLogResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.HistorySequenceNumberResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.LinkStatusIndication;
import com.accu_chek.solo_m.rcapp.application.ble.response.LinkStatusResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.OOBRequestIndication;
import com.accu_chek.solo_m.rcapp.application.ble.response.ReadHistoryResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.ReadRecordResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.RemoteBdCauseResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.RemoteBdTypeCauseResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.ResetConnectionResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.RunTimeTestResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.ScanInfoIndication;
import com.accu_chek.solo_m.rcapp.application.ble.response.ServiceDiscoveryIndication;
import com.accu_chek.solo_m.rcapp.application.ble.response.SystemSyncResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.UnknownErrorIndication;
import com.accu_chek.solo_m.rcapp.application.ble.response.WatchDogChallengeCfmResponse;
import com.accu_chek.solo_m.rcapp.application.ble.response.WatchDogChallengeIndication;
import com.accu_chek.solo_m.rcapp.application.ble.response.WatchDogChallengeResponse;


/**
 * The class defines intent action names of reponses of requests and control points for broadcasting.
 * @author EDLiu
 *
 */
public final class ResponseAction {
	public static final String EXTRA_RESPONSEPACK		= "com.accu_chek.solo_m.rcapp.application.RESPONSEPACK";
	public static final String EXTRA_CONTROLPOINTPACK	= "com.accu_chek.solo_m.rcapp.application.CONTROLPOINTPACK";
//	private static final String PACKAGE = "com.accu_chek.solo_m.rcapp.application.ble.response";
	
	
	public static final class CommandResponse{
		
	    public static final String   COMM_NO_CODE           = "";
		public static final String	 COMM_READY_IND 		= CommsReadyIndication.class.getName();
		public static final String	 COMM_CONFIG_DATA 		= ConfigurationDataResponse.class.getName();
		public static final String	 COMM_GET_INFO 			= CommsInfoResponse.class.getName();
		public static final String	 COMM_DATA_SENT 		= CommandDataSentIndication.class.getName();
	    public static final String   COMM_SET_FLIGHT_MODE   = CauseOnlyResponse.class.getName();
	    public static final String   COMM_BG_MODE           = CauseOnlyResponse.class.getName();    
	    public static final String   COMM_PRE_POWER_OFF     = CauseOnlyResponse.class.getName();
	    public static final String   COMM_UI_STATE_UPDATE   = "";
        public static final String   COMM_BT_RMV_BOND       = RemoteBdTypeCauseResponse.class.getName(); 
        public static final String   COMM_FLIGHT_MODE_STATUS= CommandDataSentIndication.class.getName();
        
        public static final String   CONFIG_REQ_IND         = "";
        public static final String   CONFIG_REQ_CFM         = "";
        public static final String   COMM_GET_ERROR_LOG     = ErrorLogResponse.class.getName();
        
        public static final String   RUNTIME_TEST           = RunTimeTestResponse.class.getName();
        public static final String   RESERVOIR_TEST         = "";

        public static final String   WATCHDOG_CHAL_REQ      = WatchDogChallengeResponse.class.getName();
        public static final String   WATCHDOG_CHAL_IND      = WatchDogChallengeIndication.class.getName();
        public static final String   WATCHDOG_CHAL_CFM      = WatchDogChallengeCfmResponse.class.getName();
        public static final String   WATCHDOG_SUSPEND       = "";
        
	    public static final String	 BT_UNKNOWN 			= UnknownErrorIndication.class.getName();
		public static final String	 BT_INTERNAL_EVENT 		= BTInternalEventNotification.class.getName();
		public static final String	 BT_DISCOVERY_SRV 		= CauseOnlyResponse.class.getName();
		public static final String	 BT_DISCOVERY_INFO 		= ServiceDiscoveryIndication.class.getName();
		public static final String	 BT_SCAN 				= CauseOnlyResponse.class.getName();
		public static final String	 BT_SCAN_INFO 			= ScanInfoIndication.class.getName();
		public static final String	 BT_CONNECT 			= ConnectResponse.class.getName();
		public static final String	 BT_DISCONNECT 			= RemoteBdCauseResponse.class.getName();
		public static final String	 BT_CONN_UPDATE 		= RemoteBdCauseResponse.class.getName();
		public static final String	 BT_PAIRABLE 			= CauseOnlyResponse.class.getName();
		public static final String	 BT_SECURITY 			= RemoteBdCauseResponse.class.getName();
		public static final String	 BT_OOB_IND				= OOBRequestIndication.class.getName();
		public static final String	 BT_OOB_CFM 			= RemoteBdCauseResponse.class.getName();
		public static final String	 BT_ATTR_READ 			= AttributeReadResponse.class.getName();
		public static final String	 BT_ATTR_WRITE 			= AttributeWriteResponse.class.getName();
		public static final String	 BT_ATTR_NOTIF_IND 		= AttributeChangeNotification.class.getName();
		
		public static final String	 BT_CCCD_CONFIG 		= CCCDResponse.class.getName();
        public static final String   BT_SYNC_STATUS_START   = "";
        public static final String   BT_SYNC_STATUS_END     = "";
        public static final String   BT_ABORT_HIST_UPDATE   = "";
        public static final String   BT_GET_PUMP_HIST       = "";
        public static final String   BT_SYNC_COUNTER_IND    = "";
        public static final String   BT_SYNC_COUNTER_CFM    = "";
        public static final String   BT_SOLOM_EMWR_IND      = BlankMessageResponse.class.getName();      
        public static final String   BT_SOLOM_EMWR_CFM      = CommandDataSentIndication.class.getName(); 
        public static final String   BT_SOLOM_CONFIG_IND    = BlankMessageResponse.class.getName();
        public static final String   BT_SOLOM_CONFIG_CFM    = CommandDataSentIndication.class.getName(); 
        public static final String   BT_TIME_SYNC_IND       = BlankMessageResponse.class.getName(); 
        public static final String   BT_TIME_SYNC_CFM       = CommandDataSentIndication.class.getName(); 
        public static final String   BT_RESET_CONNECTION    = ResetConnectionResponse.class.getName();
        public static final String   BT_SYNC_TOTAL_INSULIN_IND = BlankMessageResponse.class.getName(); 
        public static final String   BT_SYNC_TOTAL_INSULIN_CFM = CommandDataSentIndication.class.getName(); 
        public static final String   BT_BASAL_RATE_IND      = BlankMessageResponse.class.getName(); 
        public static final String   BT_BASAL_RATE_CFM      = CommandDataSentIndication.class.getName(); 
        public static final String   BT_ACTIVE_BOLUS_IND      = BlankMessageResponse.class.getName(); 
        public static final String   BT_ACTIVE_BOLUS_CFM      = CommandDataSentIndication.class.getName(); 		
        public static final String	 BT_CONNECTION_STATE	= ConnectionStateIndication.class.getName();
		public static final String	 BT_NO_CONNECTION_IND	= "";
		public static final String	 BT_NO_CONNECTION_CFM	= "";
		public static final String   BT_SYSTEM_SYNC         = SystemSyncResponse.class.getName();
		public static final String   BT_SET_TIME_STAMP      = CommandDataSentIndication.class.getName();
		public static final String   BT_CFM                 = CommandDataSentIndication.class.getName();
		
		
		public static final String	 TUNNEL_CLOSE 			= "";
		public static final String	 TUNNEL_OPEN 			= "";
		
		public static final String   PUMP_REC_READ          = ReadHistoryResponse.class.getName();
        public static final String   PUMP_REC_SEQ_READ      = HistorySequenceNumberResponse.class.getName();		
		public static final String	 PUMP_REC_ERASE_SOLOM 	= CauseOnlyResponse.class.getName();
		public static final String	 PUMP_REC_FULL_SOLOM 	= BlankMessageResponse.class.getName();
		public static final String	 PUMP_MORE_DATA			= ReadRecordResponse.class.getName();
		public static final String	 PUMP_NO_MORE_DATA		= ReadRecordResponse.class.getName();
		public static final String	 PUMP_NV_READ			= "";
		public static final String	 PUMP_NV_ERASE			= "";
		
		public static final String	 COMM_LINK_STATUS 		= LinkStatusResponse.class.getName();
		public static final String   COMM_LINK_STATUS_ID    = LinkStatusIndication.class.getName();
	}
	

	
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [BT] Fixed Klocwork issue.
