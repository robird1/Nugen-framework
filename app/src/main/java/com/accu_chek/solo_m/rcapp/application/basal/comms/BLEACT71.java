/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: BLEACT71
 * Brief: BLE Activity 71
 *
 * Create Date: 11/03/2015
 * $Revision: 25033 $
 * $Author: KiddYeh $
 * $Id: BLEACT71.java 25033 2015-11-27 10:38:45Z KiddYeh $
 */

package com.accu_chek.solo_m.rcapp.application.basal.comms;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.basal.comms.database.BdData;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallbackWithData;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ReadIDDDeviceStatus;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ResetConnection;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.E2E_Result;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeChangeNotification;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeReadResponse;
import com.accu_chek.solo_m.rcapp.application.ble.state.AbstractBLEStateHandler;
import com.accu_chek.solo_m.rcapp.application.ble.state.BLECallbackContext;
import com.accu_chek.solo_m.rcapp.application.ble.state.BLEWaitingState;
import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;


public class BLEACT71 extends AbstractBLEStateHandler
{
    private static final String TAG = "TestServiceApp";
    
    // SafetyBoolean Byte Value
    private static final byte SAFETY_TRUE = SafetyBoolean.TRUE.getByte();
    
    private static final byte SAFETY_FALSE = SafetyBoolean.FALSE.getByte();
    
    // Command State Constants
    private static final int CMD_NORMAL = 
                                    HammingDistance.SAFETY_NUMBER_VALUE_0125;
    
    private static final int CMD_RESET_RETRY = 
                                    HammingDistance.SAFETY_NUMBER_VALUE_0126;
    
    // Bit Check
    private static final int BIT0 = 0x01;
    
    // Bit Set
    private static final int BIT0_SET = 0x01;
    
    private static final byte BYTE_BIT0_SET = (byte)0x01;
    
    // Bit No Set
    private static final int NO_BIT_SET = 0x00;
    
    private static final byte BYTE_NO_BIT_SET = (byte)0x00;
    
    // Two Byte Operand
    private static final int TWO_BYTES_OPERAND = 0xFFFF;     
    
    // Bit Operation
    private static final byte BIT0_TRUE = (byte)0x01;
    
    private static final byte BIT1_TRUE = (byte)0x02;    
    
    private static final byte BIT2_TRUE = (byte)0x04;
    
    // Time Block Number in Each Data Array
    private static final byte TB_IN_ARRAY = 3;
    
    // BRP Template Number
    private static final byte BRP_TEMPLATE_NUMBER = (byte)0x01; 
    
    // ISF Profile Template Number
    private static final byte ISF_TEMPLATE_NUMBER = (byte)0xFF;
    
    // I2CHO Ratio Profile Template Number
    private static final byte I2CHO_TEMPLATE_NUMBER = (byte)0xFF;
    
    // SFLOAT Byte Number
    private static final int SFLOAT_BYTE = 2;     
    
    // Decimal Places Change Amount
    private static final int DECIMAL_PLACES_CHANGE_AMOUNT = 500;
    
    // Decimal Places
    private static final int DECIMAL_PLACE_1 = 1;
    
    private static final int DECIMAL_PLACE_2 = 2;
    
    // Time Block Array Basic Length
    private static final int TB_ARRAY_BASIC_LEN = 9;
    
    // One Time Block Data Length
    private static final int TB_DATA_LEN = 4;
    
    // BRP Template Code
    private static final byte BRP_TEMPLATE_CODE = 
            (byte)ControlPointConstant.TemplateType.BASAL_RATE_PROFILE;    
    
    // Get Template Status Response Length
    private static final int GET_TEMPLATE_STATUS_RESPONSE_LEN = 10;
    
    // Get Template Status Response Length
    private static final int RESET_TEMPLATE_STATUS_RESPONSE_LEN = 6;
    
    // Write BRP Template Response Length
    private static final int WRITE_BRP_TEMPLATE_RESPONSE_LEN = 8; 
    
    // Activate BRP Template Response Length
    private static final int ACTIVATE_BRP_TEMPLATE_RESPONSE_LEN = 8; 
    
    // Stand Response Length
    private static final int STD_RESPONSE_LEN = 8; 
    
    // Read IDD Device State Length
    private static final int READ_IDD_DEVICE_STATE_LEN = 8;
    
    // Read IDD Device State ID
    private static final int Read_IDD_Device_State_ID = 
                                      HammingDistance.SAFETY_NUMBER_VALUE_0100;
    
    // Response of ReadIDDDeviceStatus
    private ResponseReadIDDDeviceState mResponseReadIDDDeviceState = 
                                            new ResponseReadIDDDeviceState();
    
    // Response of StopDelivery
    private ResponseStopDelivery mResponseStopDelivery = 
                                            new ResponseStopDelivery();    
    
    // Response of GetBrpTemplateStatus
    private ResponseGetBrpTemplateStatus mResponseGetBrpTemplateStatus = 
                                            new ResponseGetBrpTemplateStatus();
    
    // Response of ResetBrpTemplateStatus
    private ResponseResetBrpTemplateStatus mResponseResetBrpTemplateStatus = 
                                           new ResponseResetBrpTemplateStatus();
    
    // Response of WriteBrpTemplate
    private ResponseWriteBrpTemplate mResponseWriteBrpTemplate = 
                                           new ResponseWriteBrpTemplate();
    
    // Response of ActivateBrpTemplate
    private ResponseActivateBrpTemplate mResponseActivateBrpTemplate = 
                                           new ResponseActivateBrpTemplate();    
    
    // Response of ResetConnection
    private ResponseResetConnection mResponseResetConnection = 
                                           new ResponseResetConnection();    
    
    // BLECallbackContext of ACT71
    private BLECallbackContext mACT71CbContext = null;
    
    // Command
    private int mBLECommand = 0;
    
    // Limit of BLE E2E Retry Times 
    private int mE2ERetryLimit = 5;
    
    // Limit of BLE Procedure Retry Times 
    private int mProcedureRetryLimit = 2;    
    
    // E2E Retry Times
    private int mE2ERetryCount = 0;
    
    // Procedure Retry Times
    private int mProcedureRetryCount = 0; 
    
    // Current BRP Data Array Number
    private int mCurrentArrayNum = 0;
    
    // Max BRP Data Array Number
    private int mMaxArrayNum = 0;    
    
    // Current BRP Data Array Total Number
    private int mTotalArrayNum = 0;
        
    // ArrayList of BRP Data SafetyByteArray
    private ArrayList<SafetyByteArray> mBrpDataArray = null;
    
    // Context of ACT71
    private Context mACT71RhContext = null;
    
    // ResponseCallback from Calling Function
    private ResponseCallback mCallback = null;
    
    // Command State
    private int mCmdState = CMD_NORMAL;
    
    
    /**
     * Class constructor
     * 
     * @param callbackContext [in] BLECallbackContext
     * 
     *          Callback context of this activity
     *          
     *          Range: Valid BLECallbackContext
     *          Unit: BLECallbackContext
     *          Scaling: 1
     *          
     * @param callback [in] ResponseCallback
     * 
     *          Callback for this activity
     *          
     *          Range: Valid ResponseCallback
     *          Unit: ResponseCallback
     *          Scaling: 1         
     * 
     */
    
    public BLEACT71(BLECallbackContext callbackContext, 
                    ResponseCallback callback)
    {
        super(callbackContext);
        
        // Load limit of E2E retry times from Config Matrix
        //loadE2ERetryLimit();
        
        // Load limit of procedure retry times from Config Matrix
        //loadProcedureRetryLimit();
        
        // Assign mACT71CbContext 
        mACT71CbContext = callbackContext;
        
        // Assign mACT71RhContext
        mACT71RhContext = mACT71CbContext.getBLERequestHandler().getContext();
        
        // Create BRP Data Array
        createBrpDataArray();  
        
        mTotalArrayNum = mBrpDataArray.size();
        mMaxArrayNum = mTotalArrayNum - 1; 
        
        // Assign mCallback
        mCallback = callback;
        
        // Reset error counter
        mE2ERetryCount = 0;
        mProcedureRetryCount = 0;
        
        // Start : Read IDD Device Status
        callReadIDDDeviceState();        
    }
    
    /**
     * Action when timeout happens
     * 
     * @param request [in] IBLERequest
     * 
     *          Command which timeout happens
     *          
     *          Range: Valid IBLERequest
     *          Unit: IBLERequest
     *          Scaling: 1
     *          
     * @param parameter [in] BLERequestParameter
     * 
     *          Parameter of command which timeout happens
     *          
     *          Range: Valid BLERequestParameter
     *          Unit: BLERequestParameter
     *          Scaling: 1
     * 
     * @return None
     * 
     */    
    @Override
    public void onRequestTimeout(IBLERequest request,
            BLERequestParameter parameter)
    {
        mProcedureRetryCount++;
        
        if (mProcedureRetryCount <= mProcedureRetryLimit)
        {
            callResetConnection();
        }
        else
        {
            callEmwr(EMWRList.EMW45949);
            
            setupReturnResult(SafetyBoolean.FALSE);
        }        
    }

    /**
     * Action when connection status changes
     * 
     * @param isConnected [in] SafetyBoolean
     * 
     *          Connection status
     *          
     *          SafetyBoolean.TRUE -- Connected
     *          SafetyBoolean.FALSE -- Disconnected
     *          
     *          Range: Valid SafetyBoolean
     *          Unit: SafetyBoolean
     *          Scaling: 1
     *          
     * @return None
     * 
     */    
    @Override
    public void onConnectionStateChanged(int state)
    {

        if (CommsConstant.BtState.CONNECTED == state)
        {
            if (mCmdState == CMD_RESET_RETRY)
            {
               Debug.printD(TAG, "[Reconnected] retry CMD:" + Integer.toString(mBLECommand, 16));
               
               retryCommand();
            }
            else
            {
               // Apply to the coding standard
            }
        }
    	else if ( CommsConstant.BtState.DISCONNECTED == state )
        {
            if (mCmdState != CMD_RESET_RETRY)
            {
                Debug.printD(TAG, "[CommErr] Not Connected");
                
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else
            {
                // Apply to the coding standard
            }
        }
    	else if ( CommsConstant.BtState.CONNECTIONLOST == state )
        {
            Debug.printD(TAG, "Connection Lost");
            
            setupReturnResult(SafetyBoolean.FALSE);
        }
        else
        {
            // Apply to the coding standard
        }
        
    }    
    
    /**
     * load limit of E2E retry times from Config Matrix
     * 
     * @return None
     * 
     */    
    protected void loadE2ERetryLimit()
    {
        // Key String
        SafetyString ssCmKey = null;
                
        // Get BRP max number from Config Matrix
        ssCmKey = new SafetyString(ConfigParameter.KEY_BLE_E2E_RETRY,
                CRCTool.generateCRC16(ConfigParameter.KEY_BLE_E2E_RETRY.getBytes()));
        
        mE2ERetryLimit = ReadConfig.getIntegerDataByKey(ssCmKey).get();
    } 
    
    /**
     * load limit of procedure retry times from Config Matrix
     * 
     * @return None
     * 
     */    
    protected void loadProcedureRetryLimit()
    {
        // Key String
        SafetyString ssCmKey = null;
                
        // Get BRP max number from Config Matrix
        ssCmKey = new SafetyString(ConfigParameter.KEY_BLE_PROCEDURE_RETRY,
                CRCTool.generateCRC16(ConfigParameter.KEY_BLE_PROCEDURE_RETRY.getBytes()));
        
        mProcedureRetryLimit = ReadConfig.getIntegerDataByKey(ssCmKey).get();
    }
    
    /**
     * Handle operation: read IDD device status
     * 
     * @param sbData [in] SafetyByteArray
     * 
     *          Data from MP response
     * 
     *          Range: Valid SafetyByteArray object
     *          Unit: SafetyByteArray
     *          Scaling: 1
     *          
     * @return None
     */    
    protected void handleReadIDDDeviceStatus(SafetyByteArray sbData)
    {    
        // Data byte array
        byte[] bData = sbData.getByteArray();
        ByteBuffer bbData = ByteBuffer.wrap(bData);
        
        // Therapy Control State
        byte bTCState = 0;
        
        // Operational State
        byte bOpState = 0;
        
        // Reservoir Remaining Amount
        int iRRA = 0;
        
        // Flags
        byte bFlag = 0;        
                
        // Get OpCode
        bbData.rewind();
        bbData.order(ByteOrder.LITTLE_ENDIAN);
        
        // Get therapy control state
        bTCState = bbData.get();
        
        Debug.printD(TAG, " handleReadIDDDeviceStatus TCState: " + Integer.toString(bTCState, 16));
        
        // Get operational state
        bOpState = bbData.get();
        
        Debug.printD(TAG, " handleReadIDDDeviceStatus OpState: " + Integer.toString(bOpState, 16));
        
        // Get Reservoir Remaining Amount
        iRRA = bbData.getShort();
        
        Debug.printD(TAG, " handleReadIDDDeviceStatus RRA: " + Integer.toString(iRRA, 16));
        
        // Get Flags
        bFlag = bbData.get();
        
        Debug.printD(TAG, " handleReadIDDDeviceStatus Flag: " + Integer.toString(bFlag, 16));        
                
        // Check data length
        if (bData.length != READ_IDD_DEVICE_STATE_LEN)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45941);
            
            Debug.printD(TAG, " handleReadIDDDeviceStatus NG1 ");
            
            setupReturnResult(SafetyBoolean.FALSE);          
        }
        else
        {
            // Check therapy control state
            if (bTCState == ControlPointConstant.TherapyControlState.STOP)
            {
                mE2ERetryCount = 0;
                mProcedureRetryCount = 0;

                callGetBrpTemplateStatus();
            }        
            else
            {
                mE2ERetryCount = 0;
                mProcedureRetryCount = 0;

                callStopDelivery();
            }
        }
    }    
    
    /**
     * Handle operation: stop delivery
     * 
     * @param sbData [in] SafetyByteArray
     * 
     *          Data from MP response
     * 
     *          Range: Valid SafetyByteArray object
     *          Unit: SafetyByteArray
     *          Scaling: 1
     *          
     * @return None
     */
    
    protected void handleStopDelivery(SafetyByteArray sbData)
    {    
        // Data byte array
        byte[] bData = sbData.getByteArray();
        ByteBuffer bbData = ByteBuffer.wrap(bData);
        
        // Request Code
        int iReqCode = 0;
        
        // Result Code
        byte bRCode = 0;
        
        // Opcode
        int iOpCode = 0;
                
        // Get OpCode
        bbData.rewind();
        bbData.order(ByteOrder.LITTLE_ENDIAN);
        iOpCode = bbData.getShort() & TWO_BYTES_OPERAND;
        
        Debug.printD(TAG, " handleStopDelivery OpCode: " + Integer.toString(iOpCode, 16));
        
        // Get Request Code
        iReqCode = bbData.getShort() & TWO_BYTES_OPERAND;
        
        Debug.printD(TAG, " handleStopDelivery ReqID: " + Integer.toString(iReqCode, 16));
        
        // Get Result Code
        bRCode = bbData.get();
        
        Debug.printD(TAG, " handleStopDelivery RID: " + Integer.toString(bRCode, 16));
                
        // Check data length
        if (bData.length != STD_RESPONSE_LEN)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45941);
            
            Debug.printD(TAG, " handleStopDelivery NG1 ");
            
            setupReturnResult(SafetyBoolean.FALSE);          
        }
        else
        {
            // Check Result Code
            if (bRCode != ControlPointConstant.ResponseCode.SUCCESS)
            {
                // Call EMWR
                callEmwr(EMWRList.EMW45940);
                
                Debug.printD(TAG, " handleStopDelivery NG2 ");
                
                setupReturnResult(SafetyBoolean.FALSE);
            }        
            else
            {
                mE2ERetryCount = 0;
                mProcedureRetryCount = 0;

                callGetBrpTemplateStatus();
            }
        }
    }    
    
    /**
     * Check indication of command: get BRP template status
     * 
     * @param sbData [in] SafetyByteArray
     * 
     *          Data from MP response
     * 
     *          Range: Valid SafetyByteArray object
     *          Unit: SafetyByteArray
     *          Scaling: 1
     *          
     * @return None
     */
    
    protected void handleGetBrpTemplateStatus(SafetyByteArray sbData)
    {    
        // Data byte array
        byte[] bData = sbData.getByteArray();
        ByteBuffer bbData = ByteBuffer.wrap(bData);
        
        // Configurable Flag
        int iCabFlag = 0;
        
        // Configured Flag
        int iCedFlag = 0;
        
        // Template Code
        byte bTCode = 0;
        
        // Opcode
        int iOpCode = 0;
                
        // Get OpCode
        bbData.rewind();
        bbData.order(ByteOrder.LITTLE_ENDIAN);
        iOpCode = bbData.getShort() & TWO_BYTES_OPERAND;
        
        Debug.printD(TAG, " handleGetBasalTemplateStatus OpCode: " + Integer.toString(iOpCode, 16));
        
        // Get Template Code
        bTCode = bbData.get();
        
        Debug.printD(TAG, " handleGetBasalTemplateStatus TCode: " + bTCode);
        
        // Get Configured Flag
        iCedFlag = bbData.getShort() & TWO_BYTES_OPERAND;
        
        Debug.printD(TAG, " handleGetBasalTemplateStatus CedFlag: " + iCedFlag);
        
        // Get Configurable Flag
        iCabFlag = bbData.getShort() & TWO_BYTES_OPERAND;
        
        Debug.printD(TAG, " handleGetBasalTemplateStatus CabFlag: " + iCabFlag);
        
        Debug.printD(TAG, " handleGetBasalTemplateStatus Len: " + bData.length);
        
        Debug.dumpPayload(TAG, bData);
        
        
        // Check data length
        if (bData.length != GET_TEMPLATE_STATUS_RESPONSE_LEN)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45941);
            
            Debug.printD(TAG, " handleGetBasalTemplateStatus NG1 ");
            
            setupReturnResult(SafetyBoolean.FALSE);          
        }
        else
        {
            // Check Template Code
            if (bTCode != BRP_TEMPLATE_CODE)
            {
                // Call EMWR
                callEmwr(EMWRList.EMW45940);
                
                Debug.printD(TAG, " handleGetBasalTemplateStatus NG2 ");
                
                setupReturnResult(SafetyBoolean.FALSE);
            }        
            else
            {
                checkGetBrpTemplateStatusResponse(iCedFlag, iCabFlag);
            }
        }
    }
    
    /**
     * Check response content of get BRP template status
     * 
     * @param iCedFlag [in] int
     * 
     *          Configured flag in response of command: Get Template Status
     * 
     *          Range: -2^31 to (2^31)-1 
     *          Unit: int
     *          Scaling: 1
     *          
     * @param iCabFlag [in] int
     * 
     *          Configurable flag in response of command: Get Template Status
     * 
     *          Range: -2^31 to (2^31)-1 
     *          Unit: int
     *          Scaling: 1         
     *          
     * @return None
     */
    
    protected void checkGetBrpTemplateStatusResponse(int iCedFlag,
                                                     int iCabFlag)
    {
        // BIT 0 Set Flag of Configured Flag
        int iCedFlagB0 = iCedFlag & BIT0;
        
        // BIT 0 Set Flag of Configurable Flag
        int iCabFlagB0 = iCabFlag & BIT0;

        
        if ((iCabFlagB0 == BIT0_SET) && (iCedFlagB0 == BIT0_SET)) 
        {
            mE2ERetryCount = 0;
            mProcedureRetryCount = 0;
            
            callResetBrpTemplateStatus();
        }
        else if ((iCabFlagB0 == BIT0_SET) && (iCedFlagB0 == NO_BIT_SET))
        {
            mE2ERetryCount = 0;
            mProcedureRetryCount = 0;
            
            mCurrentArrayNum = 0;
            
            callWriteBrpTemplate();
        }
        else
        {
            setupReturnResult(SafetyBoolean.FALSE);
        }
    }
    
    /**
     * Handle operation: reset BRP template status
     * 
     * @param sbData [in] SafetyByteArray
     * 
     *          Data from MP response
     * 
     *          Range: Valid SafetyByteArray object
     *          Unit: SafetyByteArray
     *          Scaling: 1
     *          
     * @return None
     */
    
    protected void handleResetBrpTemplateStatus(SafetyByteArray sbData)
    {    
        // Data byte array
        byte[] bData = sbData.getByteArray();
        ByteBuffer bbData = ByteBuffer.wrap(bData);
        
        // Template Code
        byte bTCode = 0;
        
        // Opcode
        int iOpCode = 0;
                
        // Get OpCode
        bbData.rewind();
        bbData.order(ByteOrder.LITTLE_ENDIAN);
        iOpCode = bbData.getShort() & TWO_BYTES_OPERAND;
        
        Debug.printD(TAG, " handleResetBrpTemplateStatus OpCode: " + Integer.toString(iOpCode, 16));
        
        // Get Template Code
        bTCode = bbData.get();
        
        Debug.printD(TAG, " handleResetBrpTemplateStatus TCode: " + bTCode);
        
        
        // Check data length
        if (bData.length != RESET_TEMPLATE_STATUS_RESPONSE_LEN)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45941);
            
            Debug.printD(TAG, " handleResetBrpTemplateStatus NG1 ");
            
            setupReturnResult(SafetyBoolean.FALSE);          
        }
        else
        {
            // Check Template Code
            if (bTCode != BRP_TEMPLATE_CODE)
            {
                // Call EMWR
                callEmwr(EMWRList.EMW45940);
                
                Debug.printD(TAG, " handleResetBrpTemplateStatus NG2 ");
                
                setupReturnResult(SafetyBoolean.FALSE);
            }        
            else
            {
                mE2ERetryCount = 0;
                mProcedureRetryCount = 0;
                
                mCurrentArrayNum = 0;
                
                callWriteBrpTemplate();
            }
        }
    }        
    
    /**
     * Handle operation: write BRP template
     * 
     * @param sbData [in] SafetyByteArray
     * 
     *          Data from MP response
     * 
     *          Range: Valid SafetyByteArray object
     *          Unit: SafetyByteArray
     *          Scaling: 1
     *          
     * @return None
     */
    
    protected void handleWriteBrpTemplate(SafetyByteArray sbData)
    {    
        // Data byte array
        byte[] bData = sbData.getByteArray();
        ByteBuffer bbData = ByteBuffer.wrap(bData);
        
        byte[] bDataOri = mBrpDataArray.get(mCurrentArrayNum).getByteArray();
        ByteBuffer bbDataOri = ByteBuffer.wrap(bDataOri);
        
        // Flag
        byte bFlag = 0;
        byte bFlagOri = 0;
        
        // BRP Number
        byte bBrpNum = 0;
        byte bBrpNumOri = 0;
        
        // 1st Time Block Number
        byte bTb1stNum = 0;
        byte bTb1stNumOri = 0;
        
        // Transaction Complete Flag
        byte isTransOK = SAFETY_FALSE;
        
        // Opcode
        int iOpCode = 0;
                
        // Get OpCode
        bbData.rewind();
        bbData.order(ByteOrder.LITTLE_ENDIAN);
        iOpCode = bbData.getShort() & TWO_BYTES_OPERAND;
        
        Debug.printD(TAG, " handleWriteBrpTemplate OpCode: " + Integer.toString(iOpCode, 16));
        
        // Get Flag
        bFlag = bbData.get();
        
        Debug.printD(TAG, " handleWriteBrpTemplate Flag: " + bFlag);
        
        // Get BRP number
        bBrpNum = bbData.get();
        
        Debug.printD(TAG, " handleWriteBrpTemplate BRP Num: " + bBrpNum);
        
        // Get 1st time block number
        bTb1stNum = bbData.get();
        
        Debug.printD(TAG, " handleWriteBrpTemplate 1st TB Num: " + bTb1stNum);
        
        // Get Original Data from Data Array
        // Get OpCode
        bbDataOri.rewind();
        bbDataOri.order(ByteOrder.LITTLE_ENDIAN);
        iOpCode = bbDataOri.getShort() & TWO_BYTES_OPERAND;
        
        Debug.printD(TAG, " handleWriteBrpTemplate Pkg OpCode: " + Integer.toString(iOpCode, 16));
        
        // Get Flag
        bFlagOri = bbDataOri.get();
        
        Debug.printD(TAG, " handleWriteBrpTemplate Pkg Flag: " + bFlagOri);
        
        // Get BRP number
        bBrpNumOri = bbDataOri.get();
        
        Debug.printD(TAG, " handleWriteBrpTemplate Pkg BRP Num: " + bBrpNumOri);
        
        // Get 1st time block number
        bTb1stNumOri = bbDataOri.get();
        
        Debug.printD(TAG, " handleWriteBrpTemplate Pkg 1st TB Num: " + bTb1stNumOri);

        
        // Check data length
        if (bData.length != WRITE_BRP_TEMPLATE_RESPONSE_LEN)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45941);
            
            Debug.printD(TAG, " handleWriteBrpTemplate NG1 ");
            
            setupReturnResult(SafetyBoolean.FALSE);          
        }
        else
        {
            // Check Flag
            if (mCurrentArrayNum < mMaxArrayNum)
            {
                if (bFlag == BYTE_NO_BIT_SET)
                {
                    isTransOK = SAFETY_TRUE;
                }
                else
                {
                    isTransOK = SAFETY_FALSE;
                }
            }
            else
            {
                if (bFlag == BYTE_BIT0_SET)
                {
                    isTransOK = SAFETY_TRUE;
                }
                else
                {
                    isTransOK = SAFETY_FALSE;
                }
            }
            
            // Check all data
            if ((bBrpNum != bBrpNumOri) || (bTb1stNum != bTb1stNumOri) 
                || (isTransOK != SAFETY_TRUE))
            {
                // Call EMWR
                callEmwr(EMWRList.EMW45940);
                
                Debug.printD(TAG, " handleWriteBrpTemplate NG2 ");
                
                setupReturnResult(SafetyBoolean.FALSE);
            }        
            else
            {
                mE2ERetryCount = 0;
                mProcedureRetryCount = 0;
                
                mCurrentArrayNum++;
                
                if (mCurrentArrayNum < mTotalArrayNum)
                {
                    callWriteBrpTemplate();
                }
                else
                {
                    callActivateBrpTemplate();
                }
            }
        }
    }  
    
    /**
     * Handle operation: activate BRP template
     * 
     * @param sbData [in] SafetyByteArray
     * 
     *          Data from MP response
     * 
     *          Range: Valid SafetyByteArray object
     *          Unit: SafetyByteArray
     *          Scaling: 1
     *          
     * @return None
     */
    
    protected void handleActivateBrpTemplate(SafetyByteArray sbData)
    {    
        // Data byte array
        byte[] bData = sbData.getByteArray();
        ByteBuffer bbData = ByteBuffer.wrap(bData);
        
        // BRP Template ID
        byte bBrpID = 0;
        
        // ISF Profile Template ID
        byte bIsfID = 0;
        
        // I2CHO Ratio Profile Template ID
        byte bI2choID = 0;
        
        // Opcode
        int iOpCode = 0;
                
        // Get OpCode
        bbData.rewind();
        bbData.order(ByteOrder.LITTLE_ENDIAN);
        iOpCode = bbData.getShort() & TWO_BYTES_OPERAND;
        
        Debug.printD(TAG, " handleActivateBrpTemplate OpCode: " + Integer.toString(iOpCode, 16));
        
        // Get BRP ID
        bBrpID = bbData.get();
        
        Debug.printD(TAG, " handleActivateBrpTemplate BrpID: " + bBrpID);
        
        // Get ISF ID
        bIsfID = bbData.get();
        
        Debug.printD(TAG, " handleActivateBrpTemplate IsfID: " + bIsfID);
        
        // Get I2CHO ID
        bI2choID = bbData.get();
        
        Debug.printD(TAG, " handleActivateBrpTemplate I2choID: " + bI2choID);        
        
        // Check data length
        if (bData.length != ACTIVATE_BRP_TEMPLATE_RESPONSE_LEN)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45941);
            
            Debug.printD(TAG, " handleActivateBrpTemplateStatus NG1 ");
            
            setupReturnResult(SafetyBoolean.FALSE);          
        }
        else
        {
            // Check Template Code
            if ((bBrpID != BRP_TEMPLATE_NUMBER)
                || (bIsfID != ISF_TEMPLATE_NUMBER)
                || (bI2choID != I2CHO_TEMPLATE_NUMBER))
            {
                // Call EMWR
                callEmwr(EMWRList.EMW45940);
                
                Debug.printD(TAG, " handleActivateBrpTemplateStatus NG2 ");
                
                setupReturnResult(SafetyBoolean.FALSE);
            }        
            else
            {
                setupReturnResult(SafetyBoolean.TRUE);
            }
        }
    }    
    
    /**
     * Handle E2E Error
     * 
     * @param e2e_result [in] int
     * 
     *          Data from MP response
     * 
     *          Range: Valid ResponsePack object
     *          Unit: ResponsePack
     *          Scaling: 1
     *          
     * @return None
     * 
     */    
    protected void handleE2EError(int e2e_result)
    {        
        switch(e2e_result)
        {
        case E2E_Result.CRC_ERROR:
            /* falls through */
        case E2E_Result.UNKNOWN:
            mE2ERetryCount++;
            
            if (mE2ERetryCount <= mE2ERetryLimit)
            {
                retryCommand();
            }
            else
            {
                callEmwr(EMWRList.EMW45947);
                
                setupReturnResult(SafetyBoolean.FALSE);
            }
            
            break;
            
        case E2E_Result.COUNTER_ERROR:
            mE2ERetryCount++;
            
            if (mE2ERetryCount <= mE2ERetryLimit)
            {
                callResetConnection();
            }
            else
            {
                callEmwr(EMWRList.EMW45947);
                
                setupReturnResult(SafetyBoolean.FALSE);
            }            
            
            break;
            
        default:
            // Apply to the coding standard
            break;
        }
    }  
    
    /**
     * Retry comms command
     *          
     * @return None
     * 
     */    
    protected void retryCommand()
    {
        switch(mBLECommand)
        {
        case Read_IDD_Device_State_ID:
            Debug.printD(TAG, "[Retry CMD] Read_IDD_Device_State_ID E2E:" + mE2ERetryCount
                              + "TO:" + mProcedureRetryCount);
            
            callReadIDDDeviceState();
            
            break;        
        
        case ControlPointConstant.OpCode.IDSCMD_SET_THERAPY_CONTROL_STATE:
            Debug.printD(TAG, "[Retry CMD] Stop Delivery E2E:" + mE2ERetryCount
                              + "TO:" + mProcedureRetryCount);
            
            callStopDelivery();
            
            break;
        
        case ControlPointConstant.OpCode.IDSCMD_GET_TMPL_STATUS:
            Debug.printD(TAG, "[Retry CMD] Get BRP Template Status E2E:" + mE2ERetryCount
                              + "TO:" + mProcedureRetryCount);
            
            callGetBrpTemplateStatus();
            
            break;
            
        case ControlPointConstant.OpCode.IDSCMD_RESET_TMPL_STATUS:
            Debug.printD(TAG, "[Retry CMD] Reset BRP Template Status E2E:" + mE2ERetryCount
                              + "TO:" + mProcedureRetryCount);
            
            callResetBrpTemplateStatus();
            
            break;            
            
        case ControlPointConstant.OpCode.IDSCMD_WRITE_BASAL_RATE_PROFILE:
            Debug.printD(TAG, "[Retry CMD] Write BRP Template E2E:" + mE2ERetryCount
                              + "TO:" + mProcedureRetryCount);
            
            callWriteBrpTemplate();
            
            break;
            
        case ControlPointConstant.OpCode.IDSCMD_ACT_PROFILE_TMPL:
            Debug.printD(TAG, "[Retry CMD] Active BRP Template E2E:" + mE2ERetryCount
                              + "TO:" + mProcedureRetryCount);
            
            callActivateBrpTemplate();
            
            break;
            
        default:
            // Apply to the coding standard
            break;
        }
    }
    
    /**
     * Call command: read IDD device status
     *          
     * @return None
     * 
     */    
    protected void callReadIDDDeviceState()
    {
        Debug.printD(TAG, "[Call CMD] Read IDD Device Status ");
        
        // Run Command
        mACT71CbContext.getBLERequestHandler()
                       .addRequest(
                               ReadIDDDeviceStatus.getInstance(mACT71RhContext), 
                               null, mResponseReadIDDDeviceState);
        
        mBLECommand = Read_IDD_Device_State_ID;
        
        mCmdState = CMD_NORMAL;
      
        // Start Request
        mACT71CbContext.getBLERequestHandler().startHandleRequest();
    }  
    
    /**
     * Call command: stop delivery
     *          
     * @return None
     * 
     */    
    
    protected void callStopDelivery()
    {
        Debug.printD(TAG, "[Call CMD] Stop Delivery ");
        
        // Add Request: Stop Delivery
        mACT71CbContext.getBLERequestHandler().addRequest(
                            StopDelivery.getInstance(mACT71RhContext), 
                            null, mResponseStopDelivery);
        
        mBLECommand = ControlPointConstant.OpCode.IDSCMD_SET_THERAPY_CONTROL_STATE;
        
        mCmdState = CMD_NORMAL;
      
        // Start Request
        mACT71CbContext.getBLERequestHandler().startHandleRequest();
    }    
    
    /**
     * Call command: get BRP template status
     *          
     * @return None
     * 
     */    
    
    protected void callGetBrpTemplateStatus()
    {
        Debug.printD(TAG, "[Call CMD] Get BRP Template Status ");
        
        // Add Request: BLE_SEQ94 get basal rate template status
        mACT71CbContext.getBLERequestHandler().addRequest(
                            GetBrpTemplateStatus.getInstance(mACT71RhContext), 
                            null, mResponseGetBrpTemplateStatus);
        
        mBLECommand = ControlPointConstant.OpCode.IDSCMD_GET_TMPL_STATUS;
        
        mCmdState = CMD_NORMAL;
      
        // Start Request
        mACT71CbContext.getBLERequestHandler().startHandleRequest();
    }
    
    /**
     * Call command: reset BRP template status
     *          
     * @return None
     * 
     */    
    
    protected void callResetBrpTemplateStatus()
    {
        Debug.printD(TAG, "[Call CMD] Reset BRP Template Status ");
        
        // Add Request: BLE_SEQ95 reset basal rate template status
        mACT71CbContext.getBLERequestHandler().addRequest(
                            ResetBrpTemplateStatus.getInstance(mACT71RhContext), 
                            null, mResponseResetBrpTemplateStatus);
        
        mBLECommand = ControlPointConstant.OpCode.IDSCMD_RESET_TMPL_STATUS;
        
        mCmdState = CMD_NORMAL;
      
        // Start Request
        mACT71CbContext.getBLERequestHandler().startHandleRequest();
    } 
    
    /**
     * Call command: Write BRP template
     *          
     * @return None
     * 
     */    
    
    protected void callWriteBrpTemplate()
    {
        Debug.printD(TAG, "[Call CMD] Write BRP Template : ArrayNum" + mCurrentArrayNum);
        
        // BLERequestParameter
        BLERequestParameter parameter = new BLERequestParameter();
        
        parameter.setData(mBrpDataArray.get(mCurrentArrayNum));

        // Add Request: BLE_SEQ88 write basal rate template
        mACT71CbContext.getBLERequestHandler().addRequest(
                            WriteBrpTemplate.getInstance(mACT71RhContext), 
                            parameter, mResponseWriteBrpTemplate);
        
        mBLECommand = ControlPointConstant.OpCode
                                          .IDSCMD_WRITE_BASAL_RATE_PROFILE;
        
        mCmdState = CMD_NORMAL;
      
        // Start Request
        mACT71CbContext.getBLERequestHandler().startHandleRequest();
    } 
    
    /**
     * Call command: Activate BRP template
     *          
     * @return None
     * 
     */    
    
    protected void callActivateBrpTemplate()
    {
        Debug.printD(TAG, "[Call CMD] Activate BRP Template");
        
        
        // Add Request: BLE_SEQ89 activate basal rate template
        mACT71CbContext.getBLERequestHandler().addRequest(
                            ActivateBrpTemplate.getInstance(mACT71RhContext), 
                            null, mResponseActivateBrpTemplate);
        
        mBLECommand = ControlPointConstant.OpCode
                                          .IDSCMD_ACT_PROFILE_TMPL;
        
        mCmdState = CMD_NORMAL;
      
        // Start Request
        mACT71CbContext.getBLERequestHandler().startHandleRequest();
    }   
    
    /**
     * Call command: Reset connection
     *          
     * @return None
     * 
     */    
    protected void callResetConnection()
    {
        Debug.printD(TAG, "[Call CMD] Reset Connection");
        
        
        // Add Request: Reset Connection
        mACT71CbContext.getBLERequestHandler().addRequest(
                            ResetConnection.getInstance(mACT71RhContext), 
                            null, mResponseResetConnection);
        
        mCmdState = CMD_RESET_RETRY;
      
        // Start Request
        mACT71CbContext.getBLERequestHandler().startHandleRequest();
    }    
    
    
    /**
     * Setup command set returned result
     * 
     * @param sbResult [in] SafetyBoolean
     * 
     *          Returned result of this command set
     *          
     *          Range: Valid SafetyBoolean
     *          Unit: SafetyBoolean
     *          Scaling: 1
     *          
     * @return None
     * 
     */    
    protected void setupReturnResult(SafetyBoolean sbResult)
    {
        mCallback.onRequestCompleted(sbResult);
        
        // Set wait state to leave this activity 
        mCallbackContext.setCurrentState(
                            new BLEWaitingState(mCallbackContext));
    }
    
    /**
     * Create BRP data ArrayList for BLE communication
     *   
     * @return None
     * 
     */
    
    protected void createBrpDataArray()
    {
        // Data Base 
        BdData bdData = new BdData();
        
        // Array Flag
        SafetyBoolean isEnd = SafetyBoolean.FALSE;
        SafetyBoolean is2ndBlockExist = SafetyBoolean.FALSE;
        SafetyBoolean is3rdBlockExist = SafetyBoolean.FALSE;
        
        // Time Block Array
        SafetyByteArray arrayTb = null;
        
        // Number of Time block data arrays
        int iTbArrayNum = 0;
        
        // Time Block Number in Last Array
        int iLastTbArrayNum = 0;
        
        
        // Create ArrayList
        mBrpDataArray = new ArrayList<SafetyByteArray>();
        
        // Clear ArrayList
        mBrpDataArray.clear();
        
        // Load BRP from database
        bdData.loadBrpFromDatabase(mACT71RhContext);
        
        // Calculate Last TB array number
        iLastTbArrayNum = bdData.getNumberOfTimeBlocks() % TB_IN_ARRAY;
        
        // Calculate TB array number
        if (iLastTbArrayNum == 0)
        {
            iTbArrayNum = (int)(bdData.getNumberOfTimeBlocks() / TB_IN_ARRAY);
        }
        else
        {
            iTbArrayNum = 
                    (int)(bdData.getNumberOfTimeBlocks() / TB_IN_ARRAY) + 1;
        }
        
        // 1st Time Block Number
        byte bTb1stNumber = 0;
        
        // Time Block Number (From 0)
        int iTbNum = 0;
        
        // 1st Time Block Duration
        int iTb1stDuration = 0;
        SafetyChannel<Integer> sTb1stDuration = null;
        
        // Time Block End Time
        int iEndTime1 = 0;
        int iEndTime2 = 0;
        
        // 1st Time Block Amount 
        SafetyChannel<Integer> sTb1stAmount = null;
        
        // 2nd Time Block Duration
        int iTb2ndDuration = 0;
        SafetyChannel<Integer> sTb2ndDuration = null;

        // 2nd Time Block Amount
        SafetyChannel<Integer> sTb2ndAmount = null;
  
        // 3rd Time Block Duration
        int iTb3rdDuration = 0;
        SafetyChannel<Integer> sTb3rdDuration = null;
        
        // 3rd Time Block Amount
        SafetyChannel<Integer> sTb3rdAmount = null;
        
        // Flag
        byte isFlag = 0;
        
        
        // Make Time Block Arrays
        for (int i=1; i<=iTbArrayNum; i++)
        {
            // Determine Flag
            // Last Array
            if (i == iTbArrayNum)
            {
                isEnd = SafetyBoolean.TRUE;
                
                // Block Exists Flag
                switch (iLastTbArrayNum)
                {
                case 0:
                    
                    is2ndBlockExist = SafetyBoolean.TRUE;
                    is3rdBlockExist = SafetyBoolean.TRUE;
                    
                    break;
                case 1:
                    
                    is2ndBlockExist = SafetyBoolean.FALSE;
                    is3rdBlockExist = SafetyBoolean.FALSE;                    
                    
                    break;
                case 2:
                    
                    is2ndBlockExist = SafetyBoolean.TRUE;
                    is3rdBlockExist = SafetyBoolean.FALSE;                    
                    
                    break;
                default:
                    
                    is2ndBlockExist = SafetyBoolean.FALSE;
                    is3rdBlockExist = SafetyBoolean.FALSE;                    
                    
                    break;
                }
            }
            else
            {
                isEnd = SafetyBoolean.FALSE;
                
                is2ndBlockExist = SafetyBoolean.TRUE;
                is3rdBlockExist = SafetyBoolean.TRUE;                
            }
            
            // 1st time block number
            bTb1stNumber = (byte)((i - 1) * TB_IN_ARRAY + 1);
            
            // 1st Time Block Duration and Amount
            iTbNum = (i - 1) * TB_IN_ARRAY;
            
            if (iTbNum == 0)
            {
                iEndTime1 = 0;
                iEndTime2 = CommonUtils.getOriginValue(
                         bdData.getTimeBlock(iTbNum).getEndTime().getValueCH1(),
                         bdData.getTimeBlock(iTbNum).getEndTime().getValueCH2());
            }
            else
            {
                iEndTime1 = CommonUtils.getOriginValue(
                     bdData.getTimeBlock(iTbNum - 1).getEndTime().getValueCH1(),
                     bdData.getTimeBlock(iTbNum - 1).getEndTime().getValueCH2());
                
                iEndTime2 = CommonUtils.getOriginValue(
                        bdData.getTimeBlock(iTbNum).getEndTime().getValueCH1(),
                        bdData.getTimeBlock(iTbNum).getEndTime().getValueCH2());
            }
            
            iTb1stDuration = iEndTime2 - iEndTime1;
            sTb1stDuration = CommonUtils.getSafetyChannel(iTb1stDuration); 
            
            sTb1stAmount = bdData.getTimeBlock(iTbNum).getBasal();
            
            // 2nd Time Block Duration and Amount
            iTbNum = (i - 1) * TB_IN_ARRAY + 1;
            
            isFlag = is2ndBlockExist.getByte(); 
            
            if (isFlag == SAFETY_TRUE)
            {
                iEndTime1 = CommonUtils.getOriginValue(
                     bdData.getTimeBlock(iTbNum - 1).getEndTime().getValueCH1(),
                     bdData.getTimeBlock(iTbNum - 1).getEndTime().getValueCH2());
                
                iEndTime2 = CommonUtils.getOriginValue(
                        bdData.getTimeBlock(iTbNum).getEndTime().getValueCH1(),
                        bdData.getTimeBlock(iTbNum).getEndTime().getValueCH2());
                
                iTb2ndDuration = iEndTime2 - iEndTime1;
                sTb2ndDuration = CommonUtils.getSafetyChannel(iTb2ndDuration); 
                
                sTb2ndAmount = bdData.getTimeBlock(iTbNum).getBasal();
            }
            else
            {
                iTb2ndDuration = 0;
                sTb2ndDuration = null; 
                
                sTb2ndAmount = null;
            }
            
            // 3rd Time Block Duration and Amount
            iTbNum = (i - 1) * TB_IN_ARRAY + 2;
            
            isFlag = is3rdBlockExist.getByte(); 
            
            if (isFlag == SAFETY_TRUE)
            {
                iEndTime1 = CommonUtils.getOriginValue(
                     bdData.getTimeBlock(iTbNum - 1).getEndTime().getValueCH1(),
                     bdData.getTimeBlock(iTbNum - 1).getEndTime().getValueCH2());
                
                iEndTime2 = CommonUtils.getOriginValue(
                        bdData.getTimeBlock(iTbNum).getEndTime().getValueCH1(),
                        bdData.getTimeBlock(iTbNum).getEndTime().getValueCH2());
                
                iTb3rdDuration = iEndTime2 - iEndTime1;
                sTb3rdDuration = CommonUtils.getSafetyChannel(iTb3rdDuration); 
                
                sTb3rdAmount = bdData.getTimeBlock(iTbNum).getBasal();
            }
            else
            {
                iTb3rdDuration = 0;
                sTb3rdDuration = null; 
                
                sTb3rdAmount = null;
            }            
            
            // Create time block array
            arrayTb = createBrpTbArray(isEnd,
                                       is2ndBlockExist,
                                       is3rdBlockExist,
                                       bTb1stNumber,
                                       sTb1stDuration,
                                       sTb1stAmount,
                                       sTb2ndDuration,
                                       sTb2ndAmount,
                                       sTb3rdDuration,
                                       sTb3rdAmount);
            
            // Set time block array into ArrayList
            mBrpDataArray.add(arrayTb);
        }
    }    
    
    /**
     * Create BRP time block array for BLE communication
     * 
     * @param isEnd [in] SafetyBoolean
     * 
     *         Flag of end of package
     *         
     *         Range: Valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     *         
     * @param is2ndBlockExist [in] SafetyBoolean
     * 
     *         Flag of 2nd time block existing
     *         
     *         Range: Valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     *         
     * @param is3rdBlockExist [in] SafetyBoolean
     * 
     *         Flag of 3rd time block existing
     *         
     *         Range: Valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     *         
     * @param bTb1stNumber [in] int
     * 
     *         1st time block number in this array
     *         
     *         Range: 1 to 24
     *         Unit: int
     *         Scaling: 1
     *         
     * @param sTb1stDuration [in] SafetyChannel<Integer>
     * 
     *         Duration of 1st time block in this array
     *         
     *         Range: Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     * @param sTb1stAmount [in] SafetyChannel<Integer>
     * 
     *         Amount of 1st time block in this array
     *         
     *         Range: Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     * @param sTb2ndDuration [in] SafetyChannel<Integer>
     * 
     *         Duration of 2nd time block in this array
     *         
     *         Range: null, Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     * @param sTb2ndAmount [in] SafetyChannel<Integer>
     * 
     *         Amount of 2nd time block in this array
     *         
     *         Range: null, Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     * @param sTb3rdDuration [in] SafetyChannel<Integer>
     * 
     *         Duration of 3rd time block in this array
     *         
     *         Range: null, Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     * @param sTb3rdAmount [in] SafetyChannel<Integer>
     * 
     *         Amount of 3rd time block in this array
     *         
     *         Range: null, Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *   
     * @return SafetyByteArray [out] 
     * 
     *         Byte array sent to MP
     *         
     *         Range: Valid SafetyByteArray object
     *         Unit: SafetyByteArray
     *         Scaling: 1
     * 
     */    
    protected SafetyByteArray createBrpTbArray(SafetyBoolean isEnd,
                                                 SafetyBoolean is2ndBlockExist,
                                                 SafetyBoolean is3rdBlockExist,
                                                 byte bTb1stNumber,
                                       SafetyChannel<Integer> sTb1stDuration,
                                       SafetyChannel<Integer> sTb1stAmount,
                                       SafetyChannel<Integer> sTb2ndDuration,
                                       SafetyChannel<Integer> sTb2ndAmount,
                                       SafetyChannel<Integer> sTb3rdDuration,
                                       SafetyChannel<Integer> sTb3rdAmount)
    {
        // byte array
        byte[] bData = null;
        ByteBuffer bbData = null; 
        
        // Array Length
        int iLen = 0;
        
        // Original Value of Duration
        int iOriDuration = 0;
        
        // Flag Field
        byte bFlag = 0;
        
        // Check Result
        byte bIsEnd = isEnd.getByte();
        byte bIs2ndBlockExist = is2ndBlockExist.getByte();
        byte bIs3rdBlockExist = is3rdBlockExist.getByte();

        
        // Create Byte Array
        iLen = TB_ARRAY_BASIC_LEN;
        
        if (bIs2ndBlockExist == SAFETY_TRUE)
        {
            iLen = iLen + TB_DATA_LEN;
        }
        
        if (bIs3rdBlockExist == SAFETY_TRUE)
        {
            iLen = iLen + TB_DATA_LEN;
        }
        
        bData = new byte [iLen];
        bbData = ByteBuffer.wrap(bData);
        
        // Set endian and reset ByteBuffer position
        bbData.order(ByteOrder.LITTLE_ENDIAN);
        bbData.rewind();        
                
        // Create FlagField
        // Bit0 : End Transaction
        if (bIsEnd == SAFETY_TRUE)
        {
            bFlag = BIT0_TRUE;
        }
        
        // Bit1 : Second Time Block Present
        if (bIs2ndBlockExist == SAFETY_TRUE)
        {
            bFlag = (byte) (bFlag + BIT1_TRUE);
        }
        
        // Bit2 : Third Time Block Present
        if (bIs3rdBlockExist == SAFETY_TRUE)
        {
            bFlag = (byte) (bFlag + BIT2_TRUE);
        }
        
        // Insert OpCode
        bbData.put((byte)0xCC);
        bbData.put((byte)0x0F);
        
        // Insert FlagField
        bbData.put(bFlag);
        
        // Insert BRP Template Number
        bbData.put(BRP_TEMPLATE_NUMBER);
        
        // Insert 1st Time Block Number Index
        bbData.put(bTb1stNumber);
        
        // Insert 1st Time Block Duration
        iOriDuration = CommonUtils.getOriginValue(sTb1stDuration.getValueCH1(),
                                                  sTb1stDuration.getValueCH2());
        
        bbData.putShort((short)iOriDuration);
        
        // Insert 1st Time Block Amount
        bbData.put(safetyIntToSFloatByteArray(sTb1stAmount));
        
        // Insert 2nd Time Block Duration
        if (bIs2ndBlockExist == SAFETY_TRUE)
        {
            iOriDuration = CommonUtils.getOriginValue(
                                                sTb2ndDuration.getValueCH1(),
                                                sTb2ndDuration.getValueCH2());            
            
            bbData.putShort((short)iOriDuration);
        
            // Insert 2nd Time Block Amount
            bbData.put(safetyIntToSFloatByteArray(sTb2ndAmount));
        }
        
        // Insert 3rd Time Block Duration
        if (bIs3rdBlockExist == SAFETY_TRUE)
        {
            iOriDuration = CommonUtils.getOriginValue(
                                                sTb3rdDuration.getValueCH1(),
                                                sTb3rdDuration.getValueCH2());            
            
            bbData.putShort((short)iOriDuration);
            
            // Insert 3rd Time Block Amount
            bbData.put(safetyIntToSFloatByteArray(sTb3rdAmount));
        }
        
        return new SafetyByteArray(bData, CRCTool.generateCRC16(bData));
    }

    /**
     * SafetyInteger to SFloat Byte Array
     * 
     * @param scData [in] SafetyChannel<Integer>
     * 
     *         Data for transition
     *         
     *         Range : Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *   
     * @return byte[] [out] 
     * 
     *         Byte array of Little-endian SFLoat value 
     *         
     *         Range : Valid byte[] object
     *         Unit: byte[]
     *         Scaling: 1
     * 
     */    
    protected byte[] safetyIntToSFloatByteArray(SafetyChannel<Integer> scData)
    {
        // Byte Array
        byte[] bData = new byte [SFLOAT_BYTE];
        
        // Byte Buffer
        ByteBuffer bbData = ByteBuffer.allocate(4);
        
        // SFloat Value
        SFloat sfData = null;
        
        // Int Value
        int iOriData = 0;
        int iData = 0;
        
        // Make SFloat
        iOriData = CommonUtils.getOriginValue(scData.getValueCH1(), 
                                              scData.getValueCH2());
        
        if (iOriData == 0)
        {
            iData = 0x0000;
        }
        else
        {
            if (iOriData >= DECIMAL_PLACES_CHANGE_AMOUNT)
            {
                iOriData = iOriData / 10;
                sfData = new SFloat(CommonUtils.getSafetyChannel(iOriData), 
                                    DECIMAL_PLACE_1);
            }
            else
            {
                sfData = new SFloat(scData, DECIMAL_PLACE_2);
            }
            
            iData = sfData.getValue().get();
        }
        
        // Set endian and reset position 
        bbData.order(ByteOrder.LITTLE_ENDIAN);
        bbData.rewind();
        
        // Insert data to byte array
        bbData.putInt(iData);
        
        // Get First 2 bytes of byte array
        bbData.rewind();
        bbData.get(bData);
        
        return bData;
    }    
    
    /**
     * Call EMWR for error handling
     * 
     * @param ErrorCode [in] EMWRList
     * 
     *          Internal Error Code Enum Value of Basal Delivery 
     * 
     *          Range: Valid EMWRList
     *          Unit: EMWRList
     *          Scaling: 1
     * 
     * @return None
     */    
    protected void callEmwr(EMWRList ErrorCode)
    {    
        NotifyMessage msg = new NotifyMessage(ErrorCode);
     
        NotifyProxy.showEMWR(msg);
    }    
    
    
    /**
     * Response call back for Read IDD Device Status
     * 
     */        
    private class ResponseReadIDDDeviceState implements ResponseCallbackWithData
    {                
        /**
         * Action when command response is returned
         * 
         * @param result [in] SafetyBoolean
         * 
         *        Result of sending command
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         *        
         * @param pack [in] ResponsePack
         * 
         *        Data of indication
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         * 
         * @return None
         * 
         */        
        @Override
        public void onRequestCompleted(SafetyBoolean result, ResponsePack pack)
        {
            // Result byte value
            byte isSuccess = result.getByte();
            
            // AttributeChangeNotification Indication
            AttributeReadResponse attResponse = null;            
            
            if ((isSuccess != SAFETY_TRUE) && (pack == null))
            {
                callEmwr(EMWRList.EMW45946);
                
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else if (isSuccess != SAFETY_TRUE)
            {
                attResponse = (AttributeReadResponse) pack.getResponse();
                
                handleE2EError(attResponse.getResult().get());
            }
            else
            {
                attResponse = (AttributeReadResponse) pack.getResponse();
            
                handleReadIDDDeviceStatus(attResponse.getData());
            }
        }
        
        /**
         * Action when command response is returned.
         * No use in this response
         * 
         * @param result [in] SafetyBoolean
         * 
         *        Result of sending command
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         * 
         * @return None
         * 
         */        
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            // No functionality
        }
    }    
    
    
    /**
     * Response call back for StopDelivery 
     * 
     */        
    private class ResponseStopDelivery implements ResponseCallbackWithData
    {                
        /**
         * Action when command response is returned
         * 
         * @param result [in] SafetyBoolean
         * 
         *        Result of sending command
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         *        
         * @param pack [in] ResponsePack
         * 
         *        Data of indication
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         * 
         * @return None
         * 
         */        
        @Override
        public void onRequestCompleted(SafetyBoolean result, ResponsePack pack)
        {
            // Result byte value
            byte isSuccess = result.getByte();
            
            // AttributeChangeNotification Indication
            AttributeChangeNotification attResponse = null;            
            
            if ((isSuccess != SAFETY_TRUE) && (pack == null))
            {
                callEmwr(EMWRList.EMW45946);
                
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else if (isSuccess != SAFETY_TRUE)
            {
                attResponse = (AttributeChangeNotification) pack.getResponse();
                
                handleE2EError(attResponse.getResult().get());
            }
            else
            {
                attResponse = (AttributeChangeNotification) pack.getResponse();
            
                handleStopDelivery(attResponse.getData());
            }
        }
        
        /**
         * Action when command response is returned.
         * No use in this response
         * 
         * @param result [in] SafetyBoolean
         * 
         *        Result of sending command
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         * 
         * @return None
         * 
         */        
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            // No functionality
        }
    }    
    
    
    /**
     * Response call back for GetBrpTemplateStatus 
     * 
     */        
    private class ResponseGetBrpTemplateStatus implements ResponseCallbackWithData
    {                
        /**
         * Action when command response is returned
         * 
         * @param result [in] SafetyBoolean
         * 
         *        Result of sending command
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         *        
         * @param pack [in] ResponsePack
         * 
         *        Data of indication
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         * 
         * @return None
         * 
         */        
        @Override
        public void onRequestCompleted(SafetyBoolean result, ResponsePack pack)
        {
            // Result byte value
            byte isSuccess = result.getByte();
            
            // AttributeChangeNotification Indication
            AttributeChangeNotification attResponse = null;            
            
            if ((isSuccess != SAFETY_TRUE) && (pack == null))
            {
                callEmwr(EMWRList.EMW45946);
                
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else if (isSuccess != SAFETY_TRUE)
            {
                attResponse = (AttributeChangeNotification) pack.getResponse();
                
                handleE2EError(attResponse.getResult().get());
            }
            else
            {
                attResponse = (AttributeChangeNotification) pack.getResponse();
            
                handleGetBrpTemplateStatus(attResponse.getData());
            }
        }
        
        /**
         * Action when command response is returned.
         * No use in this response
         * 
         * @param result [in] SafetyBoolean
         * 
         *        Result of sending command
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         * 
         * @return None
         * 
         */        
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            // No functionality
        }
    } 
    
    /**
     * Response call back for ResetBrpTemplateStatus 
     * 
     */        
    private class ResponseResetBrpTemplateStatus implements ResponseCallbackWithData
    {        
        /**
         * Action when command response is returned
         * 
         * @param result [in] SafetyBoolean
         * 
         *        Result of sending command
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         *        
         * @param pack [in] ResponsePack
         * 
         *        Data of indication
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         * 
         * @return None
         * 
         */        
        @Override
        public void onRequestCompleted(SafetyBoolean result, ResponsePack pack)
        {
            // Result byte value
            byte isSuccess = result.getByte();
            
            // AttributeChangeNotification Indication
            AttributeChangeNotification attResponse = null;            
            
            if ((isSuccess != SAFETY_TRUE) && (pack == null))
            {
                callEmwr(EMWRList.EMW45946);
                
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else if (isSuccess != SAFETY_TRUE)
            {
                attResponse = (AttributeChangeNotification) pack.getResponse();
                
                handleE2EError(attResponse.getResult().get());
            }
            else
            {
                attResponse = (AttributeChangeNotification) pack.getResponse();
            
                handleResetBrpTemplateStatus(attResponse.getData());
            }
        }
        
        /**
         * Action when command response is returned.
         * No use in this response
         * 
         * @param result [in] SafetyBoolean
         * 
         *        Result of sending command
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         * 
         * @return None
         * 
         */        
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            // No functionality
        }   
    }
    
    
    /**
     * Response call back for WriteBrpTemplate 
     * 
     */        
    private class ResponseWriteBrpTemplate implements ResponseCallbackWithData
    {        
        /**
         * Action when command response is returned
         * 
         * @param result [in] SafetyBoolean
         * 
         *        Result of sending command
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         *        
         * @param pack [in] ResponsePack
         * 
         *        Data of indication
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         * 
         * @return None
         * 
         */        
        @Override
        public void onRequestCompleted(SafetyBoolean result, ResponsePack pack)
        {
            // Result byte value
            byte isSuccess = result.getByte();
            
            // AttributeChangeNotification Indication
            AttributeChangeNotification attResponse = null;
            
            if ((isSuccess != SAFETY_TRUE) && (pack == null))
            {
                callEmwr(EMWRList.EMW45946);
                
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else if (isSuccess != SAFETY_TRUE)
            {
                attResponse = (AttributeChangeNotification) pack.getResponse();
                
                handleE2EError(attResponse.getResult().get());
            }
            else
            {
                attResponse = (AttributeChangeNotification) pack.getResponse();
            
                handleWriteBrpTemplate(attResponse.getData());
            }
        }
        
        /**
         * Action when command response is returned.
         * No use in this response
         * 
         * @param result [in] SafetyBoolean
         * 
         *        Result of sending command
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         * 
         * @return None
         * 
         */        
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            // No functionality
        }   
    }
    
    
    /**
     * Response call back for ActivateBrpTemplate 
     * 
     */        
    private class ResponseActivateBrpTemplate implements ResponseCallbackWithData
    {        
        /**
         * Action when command response is returned
         * 
         * @param result [in] SafetyBoolean
         * 
         *        Result of sending command
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         *        
         * @param pack [in] ResponsePack
         * 
         *        Data of indication
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         * 
         * @return None
         * 
         */        
        @Override
        public void onRequestCompleted(SafetyBoolean result, ResponsePack pack)
        {
            // Result byte value
            byte isSuccess = result.getByte();
            
            // AttributeChangeNotification Indication
            AttributeChangeNotification attResponse = null;
            
            if ((isSuccess != SAFETY_TRUE) && (pack == null))
            {
                callEmwr(EMWRList.EMW45946);
                
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else if (isSuccess != SAFETY_TRUE)
            {
                attResponse = (AttributeChangeNotification) pack.getResponse();
                                
                handleE2EError(attResponse.getResult().get());
            }
            else
            {
                attResponse = (AttributeChangeNotification) pack.getResponse();
            
                handleActivateBrpTemplate(attResponse.getData());
            }
        }
        
        /**
         * Action when command response is returned.
         * No use in this response
         * 
         * @param result [in] SafetyBoolean
         * 
         *        Result of sending command
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         * 
         * @return None
         * 
         */        
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            // No functionality
        }   
    }
    
    
    /**
     * Response call back for ResetConnection
     * 
     */    
    private class ResponseResetConnection implements ResponseCallback
    {
        /**
         * Action when command response is returned
         * 
         * @param result [in] SafetyBoolean
         * 
         *        Result of sending command
         * 
         *        Range: Valid SafetyBoolean object
         *        Unit: SafetyBoolean
         *        Scaling: 1
         * 
         * @return None
         * 
         */
        
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            byte isSuccess = result.getByte();
            
            if (isSuccess == SAFETY_TRUE)
            {
                // Apply to the coding standard
                Debug.printD(TAG, "[Reset Connection Send OK ] "); 
            }
            else
            {
                callEmwr(EMWRList.EMW45950);
                Debug.printD(TAG, " [Reset Connection NGGGGGGGGGGGGGG ] ");
                
                setupReturnResult(SafetyBoolean.FALSE);
            }
        }
    }    
}


/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// [New Feature] Basal Delivery comms functions
// [Update] Modify comments
// [Update] Add WaitDelivery for long time delivery time
// (R24268 2015-11-17 04:20:17 JacksonHuang)
// ----------------------------------------------------------------------------
// [Update] Add readIDDDeviceStatus and GetActveBRD for UISD spec change
