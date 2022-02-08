/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.MakeBeep
 * Brief: This class handles the IAS (beep) and its responses sequence.
 * 
 * Create Date: 2015/7/22
 * $Revision: 25198 $
 * $Author: IvanHuang $
 * $Id: MakeBeep.java 25198 2015-12-01 03:03:15Z IvanHuang $
 */

package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import android.content.Context;
import android.content.Intent;

import com.accu_chek.solo_m.rcapp.application.ble.RequestPayloadFactory;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.constant.UUID;
import com.accu_chek.solo_m.rcapp.application.ble.request.AttributeWriteTypeRequest;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeWriteResponse;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
 * This class handles the IAS (beep) and its responses sequence.
 */
public class MakeBeep implements IBLERequest
{
    private static final String TAG = "MakeBeep";

    /**
     * The instance of MakeBeep class
     */
    private static volatile MakeBeep mInstance = null;

    /**
     * The application context
     */
    private Context mContext = null;

    /**
     * The response callback
     */
    private ResponseCallback mCallback = null;

    /**
     * 
     * Get the one and only instance of the class MakeBeep.
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: A valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return mInstance : the one and only instance of the class MakeBeep
     *         Range: A valid object of MakeBeep
     *         Unit: MakeBeep
     *         Scaling: 1
     * 
     * @see mInstance: use this global variable to store the instance of the
     *      class MakeBeep
     */
    public static MakeBeep getInstance(Context context)
    {
        if (null == mInstance)
        {
            mInstance = new MakeBeep(context);
        }
        else
        {
            // Apply to the coding standard
        }
        return mInstance;
    }

    /**
     * The class constructor
     * 
     * @param context:
     *            Range: a valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * 
     * @return void [out]
     * 
     * @see mContext
     * 
     * 
     */
    protected MakeBeep(Context context)
    {
        mContext = context;
    }

    /**
     * This method is called after receiving the broadcast of
     * AttributeWriteResponse.
     * It gets the cause of response to check MakeBeep is success or not.
     * If the callback exists, it returns the result of response via callback
     * function.
     * 
     * @param context: The Context in which the receiver is running.
     *            Range: valid object of Context
     *            Unit: Context
     *            Scaling: 1
     * @param intent: The Intent being received.
     *            Range: valid object of Intent
     *            Unit: Intent
     *            Scaling: 1\
     * 
     * @return void [out]
     * 
     * @see mCallback
     * 
     */
    @Override
    public void doProcess(Context context, Intent intent)
    {
        Debug.printD(TAG, "[MakeBeep]: Response enter ");
        SafetyBoolean requestResult = SafetyBoolean.FALSE;
        ResponsePack pack = intent
                .getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);

        AttributeWriteResponse response = (AttributeWriteResponse) pack
                .getResponse();

        int cause = response.getCause().get();

        BLEResponseReceiver.getInstance(context).unregisterReceiver();

        if (0 == cause)
        {
            requestResult = SafetyBoolean.TRUE;
            Debug.printD(TAG, "[MakeBeep]: OK ");
        }
        else
        {
            Debug.printD(TAG, "[MakeBeep]: Failed ");
            // Apply to the coding standard
        }

        returnResult(requestResult);
    }

    /**
     * This method shall check if the callback exists or not.
     * If yes, it returns the response result via callback
     * 
     * @param isResult [in] the result of response of current request
     *            Range: a valid object of SafetyBoolean
     *            Unit: SafetyBoolean
     *            Scaling: 1
     * 
     * @return void [out]
     * 
     * @see mCallback
     */
    protected void returnResult(SafetyBoolean isResult)
    {
        if (null != mCallback)
        {
            mCallback.onRequestCompleted(isResult);
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * 
     * 
     * This method handles the IAS request.
     * 
     * @param parameter [in] the parameter of request
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1
     * @param callback [in] the callback function of response
     *            Range: a valid object of ResponseCallback
     *            Unit: ResponseCallback
     *            Scaling: 1
     * 
     * @return void [out]
     * 
     * @see mCallback
     * @see mContext
     */
    @Override
    public void request(BLERequestParameter parameter, ResponseCallback callback)
    {
        Debug.printD(TAG, "[MakeBeep]: Request enter ");
        // Mild Alert 0x01
        byte[] data = { 0x01 };
        int length = 0x0001;

        mCallback = callback;

        AttributeWriteTypeRequest request = (AttributeWriteTypeRequest) RequestPayloadFactory
                .getRequestPayload(CommsConstant.CommandCode.BT_ATTR_WRITE);

        SafetyByteArray sfBdAdress = GlobalTools.MPR.getMpAddress();

        if (null != request)
        {
            request.setRemoteBDAddress(sfBdAdress);

            request.setWriteType(new SafetyNumber<Integer>(
                    BlueConstant.WriteType.COMMAND,
                    -BlueConstant.WriteType.COMMAND));

            request.setAttributeHandle(new SafetyNumber<Integer>(
                    BlueConstant.HANDLE.BLANK_HANDLE,
                    -BlueConstant.HANDLE.BLANK_HANDLE));

            request.setUUID16(new SafetyNumber<Integer>(
                    UUID.UUID16.IAS_BEEP_MP, -UUID.UUID16.IAS_BEEP_MP));

            request.setAttributeLength(new SafetyNumber<Integer>(length,
                    -length));

            request.setWriteOffset(new SafetyNumber<Integer>(
                    BlueConstant.BLANK_OFFSET, -BlueConstant.BLANK_OFFSET));

            request.setData(new SafetyByteArray(data, CRCTool
                    .generateCRC16(data)));

            BLEResponseReceiver.getInstance(mContext.getApplicationContext())
                    .registerReceiver(
                            ResponseAction.CommandResponse.BT_ATTR_WRITE, this);

            BLEController.sendRequestToComms(mContext, request);
        }
        else
        {
            returnResult(SafetyBoolean.FALSE);
        }
    }

}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
// (R16943 2015-09-10 03:24:48 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
