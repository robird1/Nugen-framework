/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: BLEACT_SetTbrAdjustment
 * Brief: BLE Activity 55
 *
 * Create Date: 11/11/2015
 * $Revision: 24156 $
 * $Author: KiddYeh $
 * $Id: BLEACT55.java 24156 2015-11-16 05:50:26Z KiddYeh $
 */

package com.accu_chek.solo_m.rcapp.application.ble.state;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.basal.comms.GetActiveBRD;
import com.accu_chek.solo_m.rcapp.application.basal.comms.database.BdData;
import com.accu_chek.solo_m.rcapp.application.basal.comms.database.TbrData;
import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallbackWithData;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.GeneralConfirmation;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.ResetConnection;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant.E2E_Result;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeChangeNotification;
import com.accu_chek.solo_m.rcapp.application.ble.response.BasalRateDelivery;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class BLEACT55 extends AbstractBLEStateHandler
{
    private static final String TAG = "BLEACT55";
    
    // SafetyBoolean Byte Value
    private static final byte SAFETY_TRUE = SafetyBoolean.TRUE.getByte();
    
    private static final byte SAFETY_FALSE = SafetyBoolean.FALSE.getByte();
    
    // Command State Constants
    private static final int CMD_NORMAL = 
                                    HammingDistance.SAFETY_NUMBER_VALUE_0125;
    
    private static final int CMD_RESET_RETRY = 
                                    HammingDistance.SAFETY_NUMBER_VALUE_0126;    
    
    // Two Byte Operand
    private static final int TWO_BYTES_OPERAND = 0xFFFF;     
    
    // Bit Operation
    private static final byte NO_BIT_SET = (byte)0x00;
    
    private static final byte BIT0_SET = (byte)0x01;
    
    private static final byte BIT2_SET = (byte)0x04;
    
    // SFLOAT Byte Number
    private static final int SFLOAT_BYTE = 2;     
    
    // Decimal Places
    private static final int DECIMAL_PLACE_0 = 0;
    
    // TBR Array Length
    private static final int TBR_ARRAY_LEN = 8;
    
    // Stand Response Length
    private static final int STD_RESPONSE_LEN = 8;
    
    // Get Active BRD Response Basic Length
    private static final int GET_ACTIVE_BRD_RESPONSE_BASIC_LEN = 9;
    
    // Get Active BRD Response with TBR Length
    private static final int GET_ACTIVE_BRD_RESPONSE_TBR_LEN = 16;
    
    // Response of GetActiveBRD
    private ResponseGetActiveBRD mResponseGetActiveBRD = 
                                            new ResponseGetActiveBRD();
        
    // Response of ResetConnection
    private ResponseResetConnection mResponseResetConnection = 
                                           new ResponseResetConnection();   
    /**
     *  The response callback function of the latest request
     */
    private ResponseCallback mACT55Done = new ACT55Done();
    
    // BLECallbackContext of ACT
    private BLECallbackContext mActCbContext = null;
    
    // Limit of BLE E2E Retry Times 
    private int mE2ERetryLimit = 5;
    
    // Limit of BLE Procedure Retry Times 
    private int mProcedureRetryLimit = 2;    
    
    // E2E Retry Times
    private int mE2ERetryCount = 0;
    
    // Procedure Retry Times
    private int mProcedureRetryCount = 0;
    
    // Command
    private int mBLECommand = 0;
    
    // Change TBR Flag
    private byte mChangeTbr = SAFETY_TRUE;
    
    // Context of Activity
    private Context mActContext = null;
    
    // TBR Data Array
    private BLERequestParameter mParameter = null;
    
    // ResponseCallback from Calling Function
    private ResponseCallback mCallback = null;
    
    // Data Access 
    private BdData mBD = new BdData();
    
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
    
    public BLEACT55(BLECallbackContext callbackContext,
                                   ResponseCallback callback)
    {
        super(callbackContext);

        // Load limit of E2E retry times from Config Matrix
        //loadE2ERetryLimit();
        
        // Load limit of procedure retry times from Config Matrix
        //loadProcedureRetryLimit();
        
        // Assign mACT55CbContext 
        mActCbContext = callbackContext;
        
        // Assign mACT55RhContext
        mActContext = mActCbContext.getBLERequestHandler().getContext();
        
        // Assign mCallback
        mCallback = callback;
        
        // Reset error counter
        mE2ERetryCount = 0;
        mProcedureRetryCount = 0;
        
        // Start GetActiveBRD
        callGetActiveBRD();
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
        
        Debug.printD(TAG, "[Timeout] retry CMD:" + Integer.toString(mBLECommand, 16));
        
        if (mProcedureRetryCount <= mProcedureRetryLimit)
        {
            Debug.printD(TAG, "[Timeout] ProcedureRetryCount:" + mProcedureRetryCount);
            Debug.printD(TAG, "[Timeout] ProcedureRetryLimit:" + mProcedureRetryLimit);
            callResetConnection();
        }
        else
        {
            Debug.printD(TAG, "[Timeout] EMWR");
            callEmwr(EMWRList.EMW45949);
            
            setupReturnResult(SafetyBoolean.FALSE);
        }        
    }

    @Override
    public void onConnectionStateChanged(int state)
    {
    	if (CommsConstant.BtState.CONNECTED == state)
        {
            if (mCmdState == CMD_RESET_RETRY)
            {
               Debug.printD(TAG, "[Reconnected] retry CMD:" + Integer.toString(mBLECommand, 16));
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
     * Handle operation: get Active BRD
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
    
    protected void handleGetActiveBRD(SafetyByteArray sbData)
    {    
        // Data byte array
    	final int LENGTH_OF_OPCODE = 2;
    	final int LENGTH_OF_CRC_AND_COUNTER = 3;
        // Data byte array
        byte[] bData = sbData.getByteArray();
        Debug.printD(TAG, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
        Debug.dumpPayload(TAG, bData);
        Debug.printD(TAG, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
        byte[] dataArray = Arrays.copyOfRange(bData, LENGTH_OF_OPCODE, bData.length - LENGTH_OF_CRC_AND_COUNTER);
        Debug.dumpPayload(TAG, dataArray);
        Debug.printD(TAG, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
        BasalRateDelivery.parseBasalRateDelivery(mActContext,  new SafetyByteArray(dataArray,
        		CRCTool.generateCRC16(dataArray)));
        
        mE2ERetryCount = 0;
        mProcedureRetryCount = 0;
        callSyncBasalRateConfirm();

    }    
     
    /**
     * Call command: call confirm IDD Device status
     *          
     * @return None
     * 
     */    
    protected void callSyncBasalRateConfirm()
    {
        Debug.printD(TAG, "[Call CMD] Device Status Confirm ");

        BLERequestParameter parameter = new BLERequestParameter();
        parameter.setCommandCode(CommsConstant.CommandCode.BT_BASAL_RATE_CFM);
        mCallbackContext.getBLERequestHandler().addRequest(
                GeneralConfirmation.getInstance(mActContext), parameter, mACT55Done);
        // start to execute  
        mCallbackContext.getBLERequestHandler().startHandleRequest();
     
    }
    
    private class ACT55Done implements ResponseCallback
    {
        /**
         * This method is called after the response is received.
         * If the result is failed, it disconnects BD. 
         * 
         * 
         * @param result: the result of request
         *            Range: A valid object of SafetyBoolean
         *            Unit: SafetyBoolean
         *            Scaling: 1
         *            
         * @return void [out]            
         */
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            boolean isResultOK = 
                    (SafetyBoolean.TRUE.getByte() == result.getByte());
            
            if ( isResultOK )
            {
                Debug.printD(TAG, "[ACT55 done ] "); 
            }
            else
            {
                Debug.printD(TAG, "[ACT55 failed ] ");  
                
            }
            setupReturnResult(result);
        }
    }
    
    /**
     * Handle operation: set TBR adjustment
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
    
    protected void handleSetTbrAdj(SafetyByteArray sbData)
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
        
        Debug.printD(TAG, " handleSetTbrAdj OpCode: " + Integer.toString(iOpCode, 16));
        
        // Get Request Code
        iReqCode = bbData.getShort() & TWO_BYTES_OPERAND;
        
        Debug.printD(TAG, " handleSetTbrAdj ReqID: " + Integer.toString(iReqCode, 16));
        
        // Get Result Code
        bRCode = bbData.get();
        
        Debug.printD(TAG, " handleSetTbrAdj RID: " + Integer.toString(bRCode, 16));
                
        // Check data length
        if (bData.length != STD_RESPONSE_LEN)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45941);
            
            Debug.printD(TAG, " handleSetTbrAdj NG1 ");
            
            setupReturnResult(SafetyBoolean.FALSE);          
        }
        else
        {
            // Check Template Code
            if (bRCode != ControlPointConstant.ResponseCode.SUCCESS)
            {
                // Call EMWR
                callEmwr(EMWRList.EMW45940);
                
                Debug.printD(TAG, " handleSetTbrAdj NG2 ");
                
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
     * @param pack [in] ResponsePack
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
    
    protected void handleE2EError(ResponsePack pack)
    {
        // AttributeChangeNotification Indication
        AttributeChangeNotification 
                attResponse = (AttributeChangeNotification) pack.getResponse();
        
        // E2E Result
        int e2e_result = attResponse.getResult().get();
        
        switch(e2e_result)
        {
        case E2E_Result.CRC_ERROR:
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
        case ControlPointConstant.OpCode.IDSREAD_ACTIVE_BASAL:
            
            Debug.printD(TAG, "[Retry CMD] Get Activate BRD E2E:" + mE2ERetryCount
                              + "  TO:" + mProcedureRetryCount);
            
            callGetActiveBRD();
            
            break;                           
        default:
            
            // Apply to the coding standard
            
            break;
        }
    }    
    
    /**
     * Call command: call active basal rate delivery
     *          
     * @return None
     * 
     */    
    
    protected void callGetActiveBRD()
    {
        Debug.printD(TAG, "[Call CMD] Get Active BRD ");
        
        // Add Request: BLE_SEQ94 get basal rate template status
        mActCbContext.getBLERequestHandler().addRequest(
                            GetActiveBRD.getInstance(mActContext), 
                            null, mResponseGetActiveBRD);
        
        mBLECommand = ControlPointConstant.OpCode.IDSREAD_ACTIVE_BASAL;
        
        mCmdState = CMD_NORMAL;
      
        // Start Request
        mActCbContext.getBLERequestHandler().startHandleRequest();
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
        mActCbContext.getBLERequestHandler().addRequest(
                            ResetConnection.getInstance(mActContext), 
                            null, mResponseResetConnection);
        
        mCmdState = CMD_RESET_RETRY;
      
        // Start Request
        mActCbContext.getBLERequestHandler().startHandleRequest();
    }    
    
    /**
     * Create TBR data array for BLE communication
     *   
     * @return None
     * 
     */
    
    protected void createTbrDataArray()
    {        
        // byte array
        byte[] bData = new byte [TBR_ARRAY_LEN];
        ByteBuffer bbData = ByteBuffer.wrap(bData);
        
        // Duration
        int iDuration = 0;
        
        // load TBR
        loadTbr();
        
        // Set endian and reset ByteBuffer position
        bbData.order(ByteOrder.LITTLE_ENDIAN);
        bbData.rewind();
        
        // Insert OpCode
        bbData.put((byte)0xFF);
        bbData.put((byte)0x0F);
        
        // Insert Flags
        if(mChangeTbr == SAFETY_TRUE)
        {
            bbData.put(BIT2_SET);
        }
        else
        {
            bbData.put(NO_BIT_SET);
        }            
        
        // Insert TBR Type
        bbData.put((byte)ControlPointConstant.TBRType.RELATIVE);
        
        // Insert Percentage
        bbData.put(safetyIntToSFloatByteArray(mBD.getTbr().getPercentage()));
        
        // Insert Duration
        iDuration = CommonUtils.getOriginValue(
                                    mBD.getTbr().getDuration().getValueCH1(), 
                                    mBD.getTbr().getDuration().getValueCH2());
        
        bbData.putShort((short)iDuration);
        
        // Set data into BLERequestParameter
        
        mParameter =  new BLERequestParameter(); 
        
        mParameter.setData(new SafetyByteArray(bData, 
                                               CRCTool.generateCRC16(bData)));
    }
    
    /**
     * Load TBR
     * 
     * @return None
     * 
     */
    protected void loadTbr()
    {
        // TBR index
        int iIndex = 0;
        
        
        // Setup number of latest edited TBR
        setTbrLatestEditNumber();
        
        Debug.printE(TAG, "[load TBR] getTbrNumberNow: " + mBD.getTbrNumberNow());
        Debug.printE(TAG, "[load TBR] getValueCH1: " + mBD.getTbrNumberNow().getValueCH1());
        
        // Get TBR index
        iIndex = CommonUtils.getOriginValue(
                                    mBD.getTbrNumberNow().getValueCH1(),
                                    mBD.getTbrNumberNow().getValueCH2());
        
        // get TBR
        
        Debug.printE(TAG, "[load TBR] index: " + iIndex);
        
        if (iIndex == BdData.BASIC_TBR_ID)
        {
            getBasicTBR(mBD.createTbr());
            getBasicTBR(mBD.createTbrBackup());
        }
        else
        {
            getCustomTBR(mBD.createTbr(), mBD.getTbrNumberNow());
            getCustomTBR(mBD.createTbrBackup(), mBD.getTbrNumberNow());
        }
        
        // Check Data Integrity
        mBD.compareIntegerDataIntegrity(mBD.getTbr().getPercentage(), 
                                        mBD.getTbrBackup().getPercentage());
        
        mBD.compareIntegerDataIntegrity(mBD.getTbr().getDuration(), 
                                        mBD.getTbrBackup().getDuration());
    }
    
    /**
     * Setup number of latest edited TBR
     * 
     * @return None
     * 
     */
    protected void setTbrLatestEditNumber()
    {
        // Basic TBR running result
        byte isBasicTbrLatestEdit = SAFETY_FALSE;
        // TBR Number
        SafetyChannel<Integer> tbrIndex = null;
        // Custom TBR running result
        byte isCustomTbrLatestEdit = SAFETY_FALSE;
        // Custom TBR Index
        SafetyChannel<Integer> scTbrNum = null;
        // TBR Max Number
        int iTbrMaxNum = mBD.getTbrMaxNumber();        
        
        // Check Basic TBR running
        isBasicTbrLatestEdit = mBD.getBasicTbrLatestEdit(mActContext).getByte();
        
        Debug.printE(TAG, "[load TBR] isBasicTbrLatestEdit: " + isBasicTbrLatestEdit);
        
        if (isBasicTbrLatestEdit == SAFETY_TRUE)
        {
            // Set running TBR number
            tbrIndex = new SafetyChannel<Integer>(
                    CommonUtils
                        .encodeCH1Value(BdData.BASIC_TBR_ID),
                    CommonUtils
                        .encodeCH2Value(BdData.BASIC_TBR_ID));
        }
        else
        {
            // Get running custom TBR 
            for (int i = 1; i <= iTbrMaxNum; i++)
            {
                scTbrNum = new SafetyChannel<Integer>(
                        CommonUtils
                        .encodeCH1Value(i),
                        CommonUtils
                        .encodeCH2Value(i));
                
                isCustomTbrLatestEdit = 
                        mBD.getCustomTbrLatestEdit(mActContext, scTbrNum)
                           .getByte();
                
                Debug.printE(TAG, "[load TBR] isCustomTbrLatestEdit: " + isCustomTbrLatestEdit);
                
                if (isCustomTbrLatestEdit == SAFETY_TRUE)
                {
                    tbrIndex = scTbrNum;
                    i = iTbrMaxNum + 1;
                }
            }            
        }
        
        if (tbrIndex != null)
        {
            mBD.setTbrNumberNow(tbrIndex);
        }
        else
        {
            callEmwr(EMWRList.EMW45948);
        }
    }
    
    /**
     * Get basic TBR.
     * 
     * @param tbr [in] TemporaryBasalRate
     * 
     *          This parameter will be the member attributes "mTBR" or
     *          "mTBRbackup". This two TemporaryBasalRate objects will be 
     *          created at SCR0115_deliver_start_tbr creation period. 
     *          If the creation fails, NullPointerException will be thrown 
     *          and be caught by uncaughtException  
     * 
     *          Range: Valid TemporaryBasalRate object
     *          Unit: TemporaryBasalRate
     *          Scaling: 1
     *               *          
     * @return None
     */
    protected void getBasicTBR(TbrData tbr)
    {
        // Percentage
        SafetyChannel<Integer> scPercentage = null;
        // Duration
        SafetyChannel<Integer> scDuration = null;
        
        // Get data
        scPercentage = mBD.getBasicTbrPercentage(mActContext);
        scDuration = mBD.getBasicTbrDuration(mActContext);
        
        // Set data into object
        tbr.setPercentage(scPercentage);
        tbr.setDuration(scDuration);
    }
    
    /**
     * Get Custom TBR.
     * 
     * @param tbr [in] TemporaryBasalRate
     * 
     *          This parameter will be the member attributes "mTBR" or
     *          "mTBRbackup". This two TemporaryBasalRate objects will be 
     *          created at SCR0115_deliver_start_tbr creation period. 
     *          If the creation fails, NullPointerException will be thrown 
     *          and be caught by uncaughtException  
     * 
     *          Range: Valid TemporaryBasalRate object
     *          Unit: TemporaryBasalRate
     *          Scaling: 1
     *          
     * @param index [in] SafetyChannel<Integer>
     * 
     *          Index of Custom TBR  
     * 
     *          Range: Valid SafetyChannel<Integer>
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     *               *          
     * @return None
     */
    protected void getCustomTBR(TbrData tbr, 
                                SafetyChannel<Integer> index)
    {
        // TBR max number
        int iTbrMax = mBD.getTbrMaxNumber();
        // TBR number
        int iTbrNum = CommonUtils.getOriginValue(
                        index.getValueCH1(), 
                        index.getValueCH2());

        // Percentage
        SafetyChannel<Integer> scPercentage = null;
        // Duration
        SafetyChannel<Integer> scDuration = null;
        
        if ((iTbrNum > iTbrMax) || (iTbrNum < 1)) 
        {
            callEmwr(EMWRList.EMW45935);
        }
        else
        {
            // Apply to the coding standard
        }
        
        scPercentage = mBD.getCustomTbrPercentage(mActContext, index);
        scDuration = mBD.getCustomTbrDuration(mActContext, index);
        
        tbr.setPercentage(scPercentage);
        tbr.setDuration(scDuration);
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
            sfData = new SFloat(scData, DECIMAL_PLACE_0);
            
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
        if (null != mCallback)
        {
            mCallback.onRequestCompleted(sbResult);
        }
        else
        {
            // Apply to the coding standard
        }
        
        // Set wait state to leave this activity 
        mCallbackContext.setCurrentState(
                            new BLEWaitingState(mCallbackContext));
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
     * Response call back for GetActiveBRD
     * 
     */    
    
    private class ResponseGetActiveBRD implements ResponseCallbackWithData
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
            
            if ((isSuccess != SAFETY_TRUE) && (pack == null))
            {
                Debug.printD(TAG, "[ResponseGetActiveBRD is E2E failed & pack is null ] "); 
                
                callEmwr(EMWRList.EMW45946);
                
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else if (isSuccess != SAFETY_TRUE)
            {
                Debug.printD(TAG, "[ResponseGetActiveBRD is E2E failed ] "); 
                handleE2EError(pack);
            }
            else
            {
                Debug.printD(TAG, "[ResponseGetActiveBRD is E2E OK ] "); 
                
                AttributeChangeNotification response = 
                        (AttributeChangeNotification)pack.getResponse();
            
                handleGetActiveBRD(response.getData());
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
                Debug.printD(TAG, "[Reset Retry] retry CMD:" + Integer.toString(mBLECommand, 16));
                retryCommand();
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

