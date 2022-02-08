package com.accu_chek.solo_m.rcapp.application.ble;

import java.util.HashMap;

import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.request.AttributeReadRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.AttributeWriteTypeRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.BGModeRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.BlankMessageRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.CCCDRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.ConfigurationDataRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.ConnectRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.ConnectionUpdateRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.DisconnectRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.EraseHistoryRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.FlightModeRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.FlightModeStatusRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.LinkStatusRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.OOBConfirmation;
import com.accu_chek.solo_m.rcapp.application.ble.request.PairableRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.ReadHistoryRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.RemoveBondingRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.ScanRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.SecurityRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.ServiceDiscoveryRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.SolomRecordRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.TimeStampRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.UIStatusUpdateRequest;
import com.accu_chek.solo_m.rcapp.application.ble.request.WatchDogChallengeConfirmation;


/**
 * The factory generates instances of IRequest implementations by command code.
 * 
 */
public class RequestPayloadFactory
{

    /**
     * The map of valid classes of IRequest implementation.
     */
    private static final HashMap<Integer, Class<?>> mClassMap = new HashMap<Integer, Class<?>>();

    static
    {

        mClassMap.put(CommsConstant.CommandCode.COMM_GET_INFO,
                BlankMessageRequest.class);
        mClassMap.put(CommsConstant.CommandCode.WATCHDOG_CHAL_REQ,
                BlankMessageRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_ABORT_HIST_UPDATE,
                BlankMessageRequest.class);
        mClassMap.put(CommsConstant.CommandCode.RUNTIME_TEST,
                BlankMessageRequest.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_FLIGHT_MODE_STATUS,
                FlightModeStatusRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_SCAN, ScanRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_ATTR_READ,
                AttributeReadRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_ATTR_WRITE,
                AttributeWriteTypeRequest.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_BG_MODE,
                BGModeRequest.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_BT_RMV_BOND,
                RemoveBondingRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_CCCD_CONFIG,
                CCCDRequest.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_CONFIG_DATA,
                ConfigurationDataRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_CONN_UPDATE,
                ConnectionUpdateRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_CONNECT,
                ConnectRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_DISCONNECT,
                DisconnectRequest.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_SET_FLIGHT_MODE,
                FlightModeRequest.class);
        mClassMap.put(CommsConstant.CommandCode.PUMP_REC_READ,
                ReadHistoryRequest.class);
        mClassMap.put(CommsConstant.CommandCode.PUMP_REC_ERASE,
                EraseHistoryRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_OOB_CFM,
                OOBConfirmation.class);
        mClassMap.put(CommsConstant.CommandCode.BT_PAIRABLE,
                PairableRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_RESET_CONNECTION,
                BlankMessageRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_SECURITY,
                SecurityRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_DISCOVERY_SRV,
                ServiceDiscoveryRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_SET_TIME_STAMP,
                TimeStampRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_SYSTEM_SYNC,
                BlankMessageRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_SOLOM_CONFIG_CFM,
                BlankMessageRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_TIME_SYNC_CFM,
                BlankMessageRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_BASAL_RATE_CFM,
                BlankMessageRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_ACTIVE_BOLUS_CFM,
                BlankMessageRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_SOLOM_EMWR_CFM,
                BlankMessageRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_SYNC_TOTAL_INSULIN_CFM,
                BlankMessageRequest.class);
        mClassMap.put(CommsConstant.CommandCode.BT_SYNC_DEVICE_STATUS_CFM,
                BlankMessageRequest.class);
        mClassMap.put(CommsConstant.CommandCode.PUMP_REC_ERASE_SOLOM,
                SolomRecordRequest.class);
        mClassMap.put(CommsConstant.CommandCode.PUMP_REC_SEQ_READ,
                BlankMessageRequest.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_UI_STATE_UPDATE,
                UIStatusUpdateRequest.class);
        mClassMap.put(CommsConstant.CommandCode.WATCHDOG_CHAL_CFM,
                WatchDogChallengeConfirmation.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_PRE_POWER_OFF,
                BlankMessageRequest.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_LINK_STATUS,
                LinkStatusRequest.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_GET_ERROR_LOG,
                BlankMessageRequest.class);
        mClassMap.put(CommsConstant.CommandCode.COMM_CLEAR_MEMORY,
                BlankMessageRequest.class);
        
    }

    /**
     * Return an instance of IRequest implementation.
     * 
     * @param command The command code for constructor function to new an
     *            instance.
     *            Range: CommsConstant.CommandCode.COMM_INFO
     *            CommsConstant.CommandCode.WATCHDOG_CHAL_REQ
     *            CommsConstant.CommandCode.ABORT_HIST_UPDATE
     *            CommsConstant.CommandCode.SAFETY_RESERVOIR_TEST
     *            CommsConstant.CommandCode.SAFETY_RUNTIME_TEST
     *            CommsConstant.CommandCode.BT_SCAN
     *            CommsConstant.CommandCode.BT_ATTR_READ
     *            CommsConstant.CommandCode.BT_ATTR_WRITE
     *            CommsConstant.CommandCode.COMM_BG_MODE
     *            CommsConstant.CommandCode.BT_RMV_BOND
     *            CommsConstant.CommandCode.BT_CCCD_CONFIG
     *            CommsConstant.CommandCode.COMM_CONFIG
     *            CommsConstant.CommandCode.BT_CONN_UPDATE
     *            CommsConstant.CommandCode.BT_CONNECT
     *            CommsConstant.CommandCode.BT_DISCONNECT
     *            CommsConstant.CommandCode.COMM_FLIGHT_MODE
     *            CommsConstant.CommandCode.REC_READ_IDS
     *            CommsConstant.CommandCode.BT_OOB_CFM
     *            CommsConstant.CommandCode.BT_PAIRABLE
     *            CommsConstant.CommandCode.BT_RESET_CONNECTION
     *            CommsConstant.CommandCode.BT_SECURITY
     *            CommsConstant.CommandCode.BT_DISCOVERY_SRV
     *            CommsConstant.CommandCode.REC_ERASE_SOLOM
     *            CommsConstant.CommandCode.REC_READ_SOLOM
     *            CommsConstant.CommandCode.COMM_UI_UPDATE
     *            CommsConstant.CommandCode.WATCHDOG_CHAL_CFM
     *            Unit: Hamming Distance
     *            Scaling: 1
     * @return
     *         Range: Not NULL or NULL instance of IRequest
     *         Unit: N/A
     *         Scaling: 1
     */
    public static IRequest getRequestPayload(int command)
    {

        IRequest request = null;

        try
        {
            Class<?> clazz = mClassMap.get(command);
            request = (IRequest) clazz.getConstructor(int.class).newInstance(
                    command);
        }
        catch (Exception e)
        {
            // return null instance of IRequest
        }
        finally
        {
         // Apply to the coding standard
        }

        return request;

    }

}
// [BT] Fixed Klocwork issue.
