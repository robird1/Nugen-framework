/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: BTUtil
 * 
 * Brief:
 * 
 * Create Date: 2015/6/11
 * $Revision: 25192 $
 * $Author: KiddYeh $
 * $Id: BTUtil.java 25192 2015-12-01 02:34:08Z KiddYeh $
 */

package com.accu_chek.solo_m.rcapp.communication.uicommanddispatcher.util;

import java.util.ArrayList;

import android.content.Context;
import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.ble.RequestPack;
import com.accu_chek.solo_m.rcapp.application.ble.constant.BlueConstant;
import com.accu_chek.solo_m.rcapp.application.ble.request.AttributeWriteTypeRequest;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.communication.uicommanddispatcher.IUICommandDispatcher;

public abstract class BTUtil
{
    /**
     * TAG string for print trace
     */
    private static final String TAG = "BTUtil";

    /**
     * Status: Coding Done
     *
     * Function Description:
     * This function converts RequestPack into SafetyByteArry.
     * 
     * @param pack: The RequestPack to be transfer to Comms Sub-system.
     *          Range: Valid ResponsePack object
     *          Unit: ResponsePack object
     *          Scaling: 1
     * @return SafetyByteArray outArray: The converted Safety byte array.
     *          Rang: A valid safety byte array
     *          Unit: SafetyByteArray object
     *          Scaling: 1
     */
    public static SafetyByteArray wrapperRequest(RequestPack pack)
    {
        // Convert RequestPack to byte[]
        ArrayList<byte[]> data = new ArrayList<byte[]>();
        pack.writeToByteArrayList(data, 0);
        byte[] byteArr = ByteConverter.buildByteArray(data);
        Debug.dumpPayload(TAG, byteArr);

        // Convert byte[] to SafetyByteArray
        SafetyByteArray safetyBA = new SafetyByteArray(byteArr,
                CRCTool.generateCRC16(byteArr));

        return safetyBA;
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * This function converts ControlPointPack to safty byte array.
     * 
     * @param pack: The ControlPointPack to be transfer to Comms Sub-system.
     *            Range: Valid ControlPointPack object
     *            Unit: ControlPointPack object
     *            Scaling: 1
     * @return SafetyByteArray outArray: The converted SafetyByteArray.
     *          Rang: A valid SafetyByteArray
     *          Unit: SafetyByteArray object
     *          Scaling: 1
     */
//    public static SafetyByteArray wrapperControlPoint(ControlPointPack pack)
//    {
//        boolean isOPCodeOk = false;
//        SafetyByteArray safeByteArrs = null;
//        IControlPoint point = pack.getControlPoint();
//        int length = 0;
//        int uuid16 = 0;
//        byte[] uuid128 = new byte[BlueConstant.UUID128_LEN];
//        AttributeWriteTypeRequest request = new AttributeWriteTypeRequest();
//
//        // Get OP code
//        int opCode = point.getOpCode();
//        byte[] data = point.writeToByteArray().getByteArray();
//
//        // Get if the OP code is in ControlPointConstant.OpGroup.IDS_READ list
//        isOPCodeOk = ControlPointConstant.OpGroup.IDS_READ.contains(opCode);
//
//        //If OP code not OK
//        if( true == isOPCodeOk )
//        {
//            // Apply to the coding standard
//        }else
//        {
//            // Get if the OP code is in ControlPointConstant.OpGroup.IDS_CMD list
//            isOPCodeOk = ControlPointConstant.OpGroup.IDS_CMD.contains(opCode);
//        }
//
//        //If OP code not OK
//        if( true == isOPCodeOk )
//        {
//            // Apply to the coding standard
//        }else
//        {
//            // Get if the OP code is in ControlPointConstant.OpGroup.KEY_EXCHANGE list
//            isOPCodeOk = ControlPointConstant.OpGroup.KEY_EXCHANGE.contains(opCode);
//        }
//
//        // Check if OP code OK
//        if ( true == isOPCodeOk )
//        {
//            // Conver request to SafetyByteArray
//            length = data.length;
//            uuid16 = UUID.getUUID16(opCode);
//            uuid128 = UUID.getUUID128(opCode);
//
//            request = (AttributeWriteTypeRequest) RequestPayloadFactory
//                    .getRequestPayload(CommsConstant.CommandCode.BT_ATTR_WRITE);
//
//            safeByteArrs = setRequest(request, length, uuid16, uuid128, data);
//
//        }
//        else
//        {
//            // Apply to the coding standard
//        }
//
//        return safeByteArrs;
//
//    }

    /**
     * Status: Coding Done
     * 
     * Function Description:
     * This is the interface function for sending request to communication sub-system. 
     * 
     * @param context: The client's Context.
     *          Range: valid Context object
     *          Unit: Context object
     *          Scaling: 1
     * @param sba: A SafetyByte for send to Comms Sub-system.
     *          Range: Valid SafetyByteArray object
     *          Unit: SafetyByteArray object
     *          Scaling: 1
     * @return int isOk: Is ok to send request.
     *          Range: HammingDistance.SAFETY_BOOLEAN_TRUE 
     *                  / HammingDistance.SAFETY_BOOLEAN_FALSE
     *          Unit: int
     *          Scaling: 1 
     */
    public static int sendPackToSubsystem(Context context, SafetyByteArray sba)
    {
        // Get UICommandDispatcher
        IUICommandDispatcher uiCommandDispatcher = CustJavaFrameworkManager.getUICommandDispatcher(context);
        int ret = -1;

        // Check if UICommandDispatcher is valid
        if (null != uiCommandDispatcher)
        {
            try
            {
                // Submit request to UICommandDispatcher
                ret = uiCommandDispatcher.submitRequest(sba);
            }
            catch (RemoteException e)
            {
                // Show EMWR E57_E_RC_ELECTRONIC
                Debug.printD(TAG, "E57,EMW45601 Send command error!");
                NotifyMessage msg = new NotifyMessage(EMWRList.EMW45601);
                NotifyProxy.showEMWR( context, msg);

                e.printStackTrace();
            }
            finally
            {
                // Apply to the coding standard
            }

        }
        else
        {
         // Apply to the coding standard
        }
        return ret;
    }

    /**
     * Status: Coding Done
     *
     * Function Description:
     * This function converts the AttributeWriteTypeRequest and data into SafetyByteArray.
     * 
     * @param request: The Attribute Write Type Request.
     *            Range: A valid AttributeWriteTypeRequest object
     *            Unit: AttributeWriteTypeRequest object
     *            Scaling: 1
     * @param length: Data Length.
     *            Range: -2^31 to (2^31)-1
     *            Unit: int
     *            Scaling: 1
     * @param uuid16: UUID16 Operation Code.
     *            Range: 0 to (2^16)-1
     *            Unit: int
     *            Scaling: 1
     * @param uuid128: UUID128 Operation Code.
     *            Range: Valid object of byte[]
     *            Unit: byte[]
     *            Scaling: 1
     * @param data: Attribute Write Type Request Data.
     *            Range: Valid object of byte[]
     *            Unit: byte[]
     *            Scaling: 1
     * @return SafetyByteArray outArray: Wrapped SafetyByteArray of Request data.
     *            Range: Valid object of SafetyByteArray
     *            Unit: SafetyByteArray
     *            Scaling: 1
     */
    protected static SafetyByteArray setRequest(
            AttributeWriteTypeRequest request, int length, int uuid16,
            byte[] uuid128, byte[] data)
    {
        RequestPack rpack = null;
        SafetyByteArray safeByteArrs = null;

        // Check if request is valid
        if (null != request)
        {
            // Set bytes into requeest
            request.setWriteType(new SafetyNumber<Integer>(
                    BlueConstant.WriteType.REQUEST,
                    -BlueConstant.WriteType.REQUEST));
            request.setAttributeHandle(new SafetyNumber<Integer>(
                    BlueConstant.BD_ADDR_LEN, -BlueConstant.BD_ADDR_LEN));
            request.setAttributeLength(new SafetyNumber<Integer>(length,
                    -length));
            request.setUUID16(new SafetyNumber<Integer>(uuid16, -uuid16));
//            request.setUUID128(new SafetyByteArray(uuid128, CRCTool
//                    .generateCRC16(uuid128)));
            request.setWriteOffset(new SafetyNumber<Integer>(
                    BlueConstant.BLANK_OFFSET, -BlueConstant.BLANK_OFFSET));
            request.setData(new SafetyByteArray(data, CRCTool
                    .generateCRC16(data)));

            // Set request into RequestPack
            rpack = new RequestPack();
            rpack.setRequest(request);

            // Wapper the request pack to safety byte array
            safeByteArrs = wrapperRequest(rpack);
        }
        else
        {
            // Apply to the coding standard
        }
        return safeByteArrs;
    }

}
