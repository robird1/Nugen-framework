package com.accu_chek.solo_m.rcapp.application.ble;


import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;

import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
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
import com.accu_chek.solo_m.rcapp.application.util.Debug;


/**
 * The factory generates instances of IResponse implementations by command code.
 * 
 */

public class ResponsePayloadFactory
{

    /**
     * The map of valid classes of IResponse implementation.
     */
    private static final HashMap<Integer, Class<?>> mClassMap = new HashMap<Integer, Class<?>>();
    
    static
    {
        mClassMap.put(CommsConstant.CommandCode.COMM_DATA_SENT,
                CommandDataSentIndication.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_READY_IND,
                CommsReadyIndication.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_GET_INFO,
                CommsInfoResponse.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_GET_ERROR_LOG,
                ErrorLogResponse.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_BG_MODE,
                CauseOnlyResponse.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_SET_FLIGHT_MODE,
                CauseOnlyResponse.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_CONFIG_DATA,
                ConfigurationDataResponse.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_PRE_POWER_OFF,
                CauseOnlyResponse.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_BT_RMV_BOND,
                RemoteBdTypeCauseResponse.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_LINK_STATUS,
                LinkStatusResponse.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_LINK_STATUS_IND,
                LinkStatusIndication.class);
        mClassMap.put(CommsConstant.CommandCode.BT_SCAN,
                CauseOnlyResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_PAIRABLE,
                CauseOnlyResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_DISCOVERY_SRV,
                CauseOnlyResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_SCAN_INFO,
                ScanInfoIndication.class);
        mClassMap.put(CommsConstant.CommandCode.BT_ATTR_NOTIF_IND,
                AttributeChangeNotification.class);
        mClassMap.put(CommsConstant.CommandCode.BT_ATTR_READ,
                AttributeReadResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_ATTR_WRITE,
                AttributeWriteResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_INTERNAL_EVENT,
                BTInternalEventNotification.class);
        mClassMap.put(CommsConstant.CommandCode.BT_TIME_SYNC_IND,
                BlankMessageResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_SOLOM_CONFIG_IND,
                BlankMessageResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_SOLOM_EMWR_IND,
                BlankMessageResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_SYNC_TOTAL_INSULIN_IND,
                BlankMessageResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_BASAL_RATE_IND,
                BlankMessageResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_ACTIVE_BOLUS_IND,
                BlankMessageResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_SYNC_DEVICE_STATUS_IND,
                BlankMessageResponse.class);
        mClassMap.put(CommsConstant.CommandCode.PUMP_REC_FULL_SOLOM,
                BlankMessageResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_CCCD_CONFIG,
                CCCDResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_CONNECTION_STATE,
                ConnectionStateIndication.class);
        mClassMap.put(CommsConstant.CommandCode.BT_CONNECT,
                ConnectResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_OOB_IND,
                OOBRequestIndication.class);
        mClassMap.put(CommsConstant.CommandCode.BT_CONN_UPDATE,
                RemoteBdCauseResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_DISCONNECT,
                RemoteBdCauseResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_SECURITY,
                RemoteBdCauseResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_RESET_CONNECTION,
                ResetConnectionResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_DISCOVERY_INFO,
                ServiceDiscoveryIndication.class);
        mClassMap.put(CommsConstant.CommandCode.BT_SYSTEM_SYNC,
                SystemSyncResponse.class);
        mClassMap.put(CommsConstant.CommandCode.BT_UNKNOWN,
                UnknownErrorIndication.class);
        mClassMap.put(CommsConstant.CommandCode.BT_DEFECT_ALERT,
                BlankMessageResponse.class);
        mClassMap.put(CommsConstant.CommandCode.PUMP_NO_MORE_DATA,
                ReadRecordResponse.class);
        mClassMap.put(CommsConstant.CommandCode.PUMP_MORE_DATA,
                ReadRecordResponse.class);
        mClassMap.put(CommsConstant.CommandCode.RUNTIME_TEST,
                RunTimeTestResponse.class);
        mClassMap.put(CommsConstant.CommandCode.WATCHDOG_CHAL_CFM,
                WatchDogChallengeCfmResponse.class);
        mClassMap.put(CommsConstant.CommandCode.WATCHDOG_CHAL_IND,
                WatchDogChallengeIndication.class);
        mClassMap.put(CommsConstant.CommandCode.WATCHDOG_CHAL_REQ,
                WatchDogChallengeResponse.class);
        mClassMap.put(CommsConstant.CommandCode.WATCHDOG_SUSPEND,
                WatchDogChallengeResponse.class);
        mClassMap.put(CommsConstant.CommandCode.PUMP_REC_SEQ_READ,
                HistorySequenceNumberResponse.class);
        mClassMap.put(CommsConstant.CommandCode.PUMP_REC_READ,
                ReadHistoryResponse.class);
    }

    /**
     * The method returns an instance of IResponse implementation.
     * 
     * @param command The command code for constructor function to new an
     *            instance.
     *            Range: CommsConstant.CommandCode.COMM_DATA_SENT
     *            CommsConstant.CommandCode.BT_SCAN
     *            CommsConstant.CommandCode.BT_PAIRABLE
     *            CommsConstant.CommandCode.COMM_FLIGHT_MODE
     *            CommsConstant.CommandCode.BT_DISCOVERY_SRV
     *            CommsConstant.CommandCode.BT_SCAN_INFO
     *            CommsConstant.CommandCode.COMM_READY
     *            CommsConstant.CommandCode.COMM_INFO
     *            CommsConstant.CommandCode.BT_ATTR_NOTIF_IND
     *            CommsConstant.CommandCode.BT_ATTR_READ
     *            CommsConstant.CommandCode.BT_ATTR_WRITE
     *            CommsConstant.CommandCode.COMM_BG_MODE
     *            CommsConstant.CommandCode.BT_INTERNAL_ERR
     *            CommsConstant.CommandCode.TIME_SYNC_IND
     *            CommsConstant.CommandCode.SOLOM_CONFIG_IND
     *            CommsConstant.CommandCode.REC_FULL_SOLOM
     *            CommsConstant.CommandCode.BT_CCCD_CONFIG
     *            CommsConstant.CommandCode.COMM_DEFECT_ALERT
     *            CommsConstant.CommandCode.COMM_CONFIG
     *            CommsConstant.CommandCode.BT_CONNECTION_STATE
     *            CommsConstant.CommandCode.BT_CONNECT
     *            CommsConstant.CommandCode.BT_OOB_IND
     *            CommsConstant.CommandCode.COMM_PRE_POWER_OFF
     *            CommsConstant.CommandCode.NO_MORE_DATA
     *            CommsConstant.CommandCode.MORE_DATA
     *            CommsConstant.CommandCode.BT_CONN_UPDATE
     *            CommsConstant.CommandCode.BT_DISCONNECT
     *            CommsConstant.CommandCode.BT_SECURITY
     *            CommsConstant.CommandCode.BT_RMV_BOND
     *            CommsConstant.CommandCode.COMM_LINK_STATUS
     *            CommsConstant.CommandCode.BT_RESET_CONNECTION
     *            CommsConstant.CommandCode.SAFETY_RUNTIME_TEST
     *            CommsConstant.CommandCode.BT_DISCOVERY_INFO
     *            CommsConstant.CommandCode.COMM_UNKNOWN
     *            CommsConstant.CommandCode.WATCHDOG_CHAL_CFM
     *            CommsConstant.CommandCode.WATCHDOG_CHAL_IND
     *            CommsConstant.CommandCode.WATCHDOG_CHAL_REQ
     *            Unit: Hamming Distance
     *            Scaling: 1
     *            
     * @return response : The instance of IResponse implementation.
     *         Range: Not NULL or NULL instance of IResponse
     *         Unit: N/A
     *         Scaling: 1
     */
    public static IResponse getResponsePayload(int command)
    {
        IResponse response = null;

        try
        {
            Class<?> clazz = mClassMap.get(command);
            response = (IResponse) clazz.getConstructor(int.class).newInstance(
                    command);

        }
        catch (NoSuchMethodException e)
        {
            e.printStackTrace();
        }
        catch (IllegalArgumentException e)
        {
            e.printStackTrace();
        }
        catch (InstantiationException e)
        {
            e.printStackTrace();
        }
        catch (IllegalAccessException e)
        {
            e.printStackTrace();
        }
        catch (InvocationTargetException e)
        {
            e.printStackTrace();
        }

        return response;
    }

}
// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
