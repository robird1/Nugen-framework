/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.state.BLEKeyExchangeProcedure
 * Brief: BLE control seq 46 KeyExchangeProcedure
 *
 * Create Date: 2015/7/21
 * $Revision: 24671 $
 * $Author: KiddYeh $
 * $Id: BLEKeyExchangeProcedure.java 24671 2015-11-24 05:28:55Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.state;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.apache.http.util.ByteArrayBuffer;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;

import com.accu_chek.solo_m.rcapp.application.ble.ResponsePack;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallbackWithData;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestHandler.IBLERequest;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEResponseReceiver;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.Disconnect;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.KeyExchagneJNI;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.SendKeyExchangeCP;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.SendKeyExchangeOnlyWriteResponse;
import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ControlPointConstant.KESOpCode;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.constant.UUID;
import com.accu_chek.solo_m.rcapp.application.ble.response.AttributeChangeNotification;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

public class BLEKeyExchangeProcedure extends AbstractBLEStateHandler
{

    private static final String TAG = "BLEKeyExchangeProcedure";
    
    // Two Byte Operand
    private static final int TWO_BYTES_OPERAND = 0xFFFF; 
    
    // SafetyBoolean Byte Value
    private static final byte SAFETY_TRUE = SafetyBoolean.TRUE.getByte();
    
    private static final byte KES_MAJOR_VERSION = 0x01;
    
    private static final byte KES_MINOR_VERSION = 0x00; 

    private static final int KEY_LENGTH = 16;
    
    private static final int M1_FINAL_BLOCK_LENGTH = 12;
    
    private static final int M1_TOTAL_LENGTH = 284;
    
    private static final int M3_FINAL_BLOCK_LENGTH = 16;
    
    private static final int M3_TOTAL_LENGTH = 144;
    
    private static final int M5_FINAL_BLOCK_LENGTH = 16;
    
    private static final int M5_TOTAL_LENGTH = 32;
    
    /**
     * KES receiving data frame for M1,M3,M5 data  
     */
    private ByteArrayBuffer mReceivedData = new ByteArrayBuffer(0);
    
    /**
     * KES sending out data frame for M2,M4,M6 data   
     */
    private ByteBuffer mSentOutData = null;
    
    // Context of Use Case
    private Context mUseCaseContext = null;
    
    /**
     *  The BondStateChangedListener callback function
     */
    private ResponseCallback mCallback = null;
    
    /**
     *  The response callback function of Disconnect
     */
    private ResponseCallback mDisconnectDone = new DisconnectDone();
    
    /**
     *  The response callback function of KESVersion request
     */
    private ResponseCallbackWithData mResponseKESVersion = new ResponseKESVersion();
    
    /**
     *  The response callback function of KESM1Blocks request
     */
    private ResponseCallbackWithData mResponseKESM1Blocks = new ResponseKESM1Blocks();

    /**
     *  The response callback function of KESM2Blocks request
     */
    private ResponseCallbackWithData mResponseKESM2Blocks = new ResponseKESM2Blocks();
    
    /**
     *  The response callback function of KESM3Blocks request
     */
    private ResponseCallbackWithData mResponseKESM3Blocks = new ResponseKESM3Blocks();

    /**
     *  The response callback function of KESM4Blocks request
     */
    private ResponseCallbackWithData mResponseKESM4Blocks = new ResponseKESM4Blocks();
    
    /**
     *  The response callback function of KESM5Blocks request
     */
    private ResponseCallbackWithData mResponseKESM5Blocks = new ResponseKESM5Blocks();

    /**
     *  The response callback function of KESM6Blocks request
     */
    private ResponseCallbackWithData mResponseKESM6Blocks = new ResponseKESM6Blocks();
    
    
    private KESReceiver mKESReceiver = new KESReceiver();
    
    /**
     * This method adds the BLEUC01 sequence requests in request list of the 
     * request handler and start to execute the request.
     * 
     * @param callbackContext: the ble state handler
     *            Range: a valid object of BLECallbackContext
     *            Unit: BLECallbackContext
     *            Scaling: 1
     * @param callback: the listener of the bond state
     *            Range: a valid object of IOnBondStateChangedListener
     *            Unit: IOnBondStateChangedListener
     *            Scaling: 1           
     * 
     * @return void [out]
     * 
     * @see mCallback
     * @see mCallbackContext
     */
    public BLEKeyExchangeProcedure(BLECallbackContext callbackContext, ResponseCallback callback)
    {
        
        super(callbackContext);
        
        mUseCaseContext = mCallbackContext.getBLERequestHandler().getContext();
        
        mCallback = callback;
        
        
        Debug.printD(TAG, "KES Version request");
        byte[] data = { ControlPointConstant.KESOpCode.VERSION_REQUEST, KES_MAJOR_VERSION, KES_MINOR_VERSION };
        
        BLERequestParameter ResquestKESVersionParameter = new BLERequestParameter();
        ResquestKESVersionParameter.setData(new SafetyByteArray(data,CRCTool.generateCRC16(data)));
        // add ReadDeviceInfo request in list
        mCallbackContext.getBLERequestHandler().addRequest(
        		SendKeyExchangeCP.getInstance(mUseCaseContext), ResquestKESVersionParameter, mResponseKESVersion);

      
        // start to execute 
        mCallbackContext.getBLERequestHandler().startHandleRequest();
        
        IntentFilter filter = new IntentFilter();
        filter.addAction(ResponseAction.CommandResponse.BT_ATTR_NOTIF_IND);
        
        mUseCaseContext.registerReceiver(mKESReceiver, filter);
    }
    
    
    
    private class KESReceiver extends BroadcastReceiver
    { 

		@Override
		public void onReceive(Context context, Intent intent) 
		{
            ResponsePack pack = intent.getParcelableExtra(BLEResponseReceiver.KEY_RESPONSE_PACK);
            
            AttributeChangeNotification attResponse = (AttributeChangeNotification) pack.getResponse();
                    
            Debug.printD(TAG," %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
            Debug.printD(TAG, "getAttribHandle = = 0x" + Integer.toHexString(attResponse.getAttribHandle().get() & 0xFFFF) );
            Debug.printD(TAG," %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
            
            //UUID 
            boolean isUUIDOK = UUID.UUID16.KEY_EXCHANGE_CP == (attResponse.getAttribHandle().get() & 0xFFFF);
            
			if (isUUIDOK)
            {
				// Indication Data
	            SafetyByteArray sbData = attResponse.getData();
	            byte[] bData = sbData.getByteArray();
	            ByteBuffer bbData = ByteBuffer.wrap(bData);
	            
	            // Get OpCode
	            bbData.rewind();
	            bbData.order(ByteOrder.LITTLE_ENDIAN);
	            int iOpCode = bbData.get();
	            
	            Debug.printD(TAG, " Opcode: " + iOpCode);
	            
	            // Operation corresponded with OpCode
	            switch(iOpCode)
	            {
	            case ControlPointConstant.KESOpCode.RESPONSE_CODE:
	            	
	            	int step = bbData.get();
	            	int stepResult = bbData.get();
	            	if (0x0F != stepResult)
	            	{
	            		setupReturnResult(SafetyBoolean.FALSE);   
	            	}
	            	else
	            	{
	            		handleKESStep(step);
	            		
	            	}
	            	

	                break;
              
	            default:
	            	break;
	                
	            }
			
		    }
    
       }

		private void handleKESStep(int step) 
		{
			switch(step)
			{
			case KESOpCode.STEP1: //step1

				byte[] data = { ControlPointConstant.KESOpCode.STEP2 };
				Debug.printD(TAG, " [KESOpCode.STEP1 done]: "); 
			    BLERequestParameter ResquestKESStep2 = new BLERequestParameter();
			    ResquestKESStep2.setData(new SafetyByteArray(data,CRCTool.generateCRC16(data)));
			    // add ReadDeviceInfo request in list
			    mCallbackContext.getBLERequestHandler().addRequest(
			    		SendKeyExchangeCP.getInstance(mUseCaseContext), ResquestKESStep2, mResponseKESM3Blocks);
			    // start to execute 
		        mCallbackContext.getBLERequestHandler().startHandleRequest();
				break;
			case KESOpCode.STEP2: //step2
			    
			    byte[] data1 = { ControlPointConstant.KESOpCode.STEP3 };
			    Debug.printD(TAG, " [KESOpCode.STEP2 done]: "); 
			    BLERequestParameter ResquestKESStep3 = new BLERequestParameter();
			    ResquestKESStep3.setData(new SafetyByteArray(data1,CRCTool.generateCRC16(data1)));
			    // add ReadDeviceInfo request in list
			    mCallbackContext.getBLERequestHandler().addRequest(
			    		SendKeyExchangeCP.getInstance(mUseCaseContext), ResquestKESStep3, mResponseKESM5Blocks);
			    // start to execute 
		        mCallbackContext.getBLERequestHandler().startHandleRequest();
				break;
			case KESOpCode.STEP3: //step3
				Debug.printD(TAG, " [KESOpCode.STEP3 done]: ");
			    setupReturnResult(SafetyBoolean.TRUE); 
			    mReceivedData.clear();
			    break;
			default:
				setupReturnResult(SafetyBoolean.FALSE); 
				break;

			}
		}
    }
    
    
    private class ResponseKESM1Blocks implements ResponseCallbackWithData
    {
        /**
         * This method is called after the response is received.
         * No use in this response 
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
           // No functionality
        }

        @Override
        public void onRequestCompleted(SafetyBoolean result, ResponsePack pack)
        {
         // Result byte value
            byte isSuccess = result.getByte();
            
            if (isSuccess != SAFETY_TRUE)
            {               
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else
            {
                AttributeChangeNotification response = 
                        (AttributeChangeNotification)pack.getResponse();
            
                SafetyByteArray sbData = response.getData();
	            byte[] bData = sbData.getByteArray();
	            ByteBuffer bbData = ByteBuffer.wrap(bData);
	            
	            // Get OpCode
	            bbData.rewind();
	            bbData.order(ByteOrder.LITTLE_ENDIAN);
	            int iOpCode = bbData.get();
	            int mOffset = bbData.getShort() & TWO_BYTES_OPERAND;
	            int mlength = bbData.get();
	            Debug.printD(TAG, " KESM1 OpCode = : " + iOpCode);
	            byte[] temp = new byte[mlength];
	            bbData.get(temp, 0, mlength);
	            
	            Debug.printD(TAG, "111111111111111111111111111111111111111111111");
	            Debug.printD(TAG,
	                    "KESM1_DATAResponse OpCode = " + iOpCode);
	            Debug.printD(TAG,
	                    "KESM1_DATAResponse M1_Offset = " + mOffset);
	            Debug.printD(TAG,
	                    "KESM1_DATAResponse M1_Length = " + mlength);
	            Debug.printD(TAG, "Temp = ");
                Debug.dumpPayload(TAG, temp);
	            Debug.printD(TAG, "111111111111111111111111111111111111111111111");
	            
	            mReceivedData.append(temp, 0, temp.length);

	            if ((M1_FINAL_BLOCK_LENGTH == mlength)
	                    && ((M1_TOTAL_LENGTH - M1_FINAL_BLOCK_LENGTH) == mOffset))
	            {
	            	byte[] entropy = new byte[32];
					byte[] nonce = new byte[16];
	                Debug.printD(TAG, "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY");
	                Debug.printD(TAG, "M1_data = ");
	                Debug.dumpPayload(TAG, mReceivedData.toByteArray());
	                Debug.printD(TAG, "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY");

	                try 
	                {
	                	String time = String.valueOf(System.currentTimeMillis());
						MessageDigest digest = MessageDigest.getInstance("SHA-256");
						
						byte[] hash = digest.digest(time.getBytes());
						
						
						ByteBuffer buffer = ByteBuffer.wrap(hash);
						buffer.get(entropy);
						
						hash = digest.digest(buffer.array());						
						buffer = ByteBuffer.wrap(hash);
						buffer.get(nonce);
						
					} 
	                catch (NoSuchAlgorithmException e) 
	                {
						e.printStackTrace();
					}
	                finally
	                {
	                	// Apply to coding standard.
	                }
	                
	                KeyExchagneJNI.Kes_init(entropy, nonce);
	                mSentOutData = ByteBuffer.wrap(KeyExchagneJNI
	                        .Kes_step1(mReceivedData.toByteArray()));

	                mReceivedData.clear();
	                
	                byte[] data = getRemainingData(mSentOutData, ControlPointConstant.KESOpCode.M2_DATA);
	                BLERequestParameter ResquestKESM2Parameter = new BLERequestParameter();
	                ResquestKESM2Parameter.setData(new SafetyByteArray(data,CRCTool.generateCRC16(data)));
	                // add ReadDeviceInfo request in list
	                mCallbackContext.getBLERequestHandler().addRequest(
	                		SendKeyExchangeOnlyWriteResponse.getInstance(mUseCaseContext), ResquestKESM2Parameter, mResponseKESM2Blocks);
	                
	                // start to execute 
	                mCallbackContext.getBLERequestHandler().startHandleRequest();
	            }
	            else
	            {
	                // Apply to the coding standard
	            }

                                              
            }

        }
  
    }
    
    
    private class ResponseKESM2Blocks implements ResponseCallbackWithData
    {
        /**
         * This method is called after the response is received.
         * No use in this response 
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
           // No functionality
        }

        @Override
        public void onRequestCompleted(SafetyBoolean result, ResponsePack pack)
        {
         // Result byte value
            byte isSuccess = result.getByte();
            
            if (isSuccess != SAFETY_TRUE)
            {               
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else
            {
            
                int remainingLength = mSentOutData.remaining();
                
                if (0 < remainingLength)
                {
              
                    byte[] data = getRemainingData(mSentOutData, ControlPointConstant.KESOpCode.M2_DATA);
	                BLERequestParameter ResquestKESM2Parameter = new BLERequestParameter();
	                ResquestKESM2Parameter.setData(new SafetyByteArray(data,CRCTool.generateCRC16(data)));
	                // add ReadDeviceInfo request in list
	                mCallbackContext.getBLERequestHandler().addRequest(
	                		SendKeyExchangeOnlyWriteResponse.getInstance(mUseCaseContext), ResquestKESM2Parameter, mResponseKESM2Blocks);
	                
	                // start to execute 
	                mCallbackContext.getBLERequestHandler().startHandleRequest();
                }
                else
                {
                    Debug.printD(TAG, "KES M2 done!");
                }
	            
                                              
            }

        }

        
    }
    
    
    private class ResponseKESM3Blocks implements ResponseCallbackWithData
    {
        /**
         * This method is called after the response is received.
         * No use in this response 
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
           // No functionality
        }

        @Override
        public void onRequestCompleted(SafetyBoolean result, ResponsePack pack)
        {
         // Result byte value
            byte isSuccess = result.getByte();
            
            if (isSuccess != SAFETY_TRUE)
            {               
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else
            {
                AttributeChangeNotification response = 
                        (AttributeChangeNotification)pack.getResponse();
            
                SafetyByteArray sbData = response.getData();
	            byte[] bData = sbData.getByteArray();
	            ByteBuffer bbData = ByteBuffer.wrap(bData);
	            
	            // Get OpCode
	            bbData.rewind();
	            bbData.order(ByteOrder.LITTLE_ENDIAN);
	            int iOpCode = bbData.get();
	            int mOffset = bbData.getShort() & TWO_BYTES_OPERAND;
	            int mlength = bbData.get();
	            Debug.printD(TAG, " KESM3 OpCode = : " + iOpCode);
	            byte[] temp = new byte[mlength];
	            bbData.get(temp, 0, mlength);
	            Debug.printD(TAG, "111111111111111111111111111111111111111111111");
	            Debug.printD(TAG,
	                    "KESM3_DATAResponse OpCode = " + iOpCode);
	            Debug.printD(TAG,
	                    "KESM3_DATAResponse M3_Offset = " + mOffset);
	            Debug.printD(TAG,
	                    "KESM3_DATAResponse M3_Length = " + mlength);

	            Debug.printD(TAG, "111111111111111111111111111111111111111111111");
	            
	            mReceivedData.append(temp, 0, temp.length);

	            if ((M3_FINAL_BLOCK_LENGTH == mlength)
	                    && ((M3_TOTAL_LENGTH - M3_FINAL_BLOCK_LENGTH) == mOffset))
	            {

	                Debug.printD(TAG, "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY");
	                Debug.printD(TAG, "M3_data = ");
	                Debug.dumpPayload(TAG, mReceivedData.toByteArray());
	                Debug.printD(TAG, "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY");

	                mSentOutData = ByteBuffer.wrap(KeyExchagneJNI
	                        .Kes_step2(mReceivedData.toByteArray(), GlobalTools.MPR.getPinCode()));
	                mReceivedData.clear();
 
	                byte[] data = getRemainingData(mSentOutData, ControlPointConstant.KESOpCode.M4_DATA);
	                BLERequestParameter ResquestKESM4Parameter = new BLERequestParameter();
	                ResquestKESM4Parameter.setData(new SafetyByteArray(data,CRCTool.generateCRC16(data)));
	                // add ReadDeviceInfo request in list
	                mCallbackContext.getBLERequestHandler().addRequest(
	                		SendKeyExchangeOnlyWriteResponse.getInstance(mUseCaseContext), ResquestKESM4Parameter, mResponseKESM4Blocks);
	                
	                // start to execute 
	                mCallbackContext.getBLERequestHandler().startHandleRequest();
	            }
	            else
	            {
	                // Apply to the coding standard
	            }

                                              
            }

        }
  
    }
    
    
    private class ResponseKESM4Blocks implements ResponseCallbackWithData
    {
        /**
         * This method is called after the response is received.
         * No use in this response 
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
           // No functionality
        }

        @Override
        public void onRequestCompleted(SafetyBoolean result, ResponsePack pack)
        {
         // Result byte value
            byte isSuccess = result.getByte();
            
            if (isSuccess != SAFETY_TRUE)
            {               
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else
            {
            
                int remainingLength = mSentOutData.remaining();
                
                if (0 < remainingLength)
                {
              
                    byte[] data = getRemainingData(mSentOutData, ControlPointConstant.KESOpCode.M4_DATA);
	                BLERequestParameter ResquestKESM4Parameter = new BLERequestParameter();
	                ResquestKESM4Parameter.setData(new SafetyByteArray(data,CRCTool.generateCRC16(data)));
	                // add ReadDeviceInfo request in list
	                mCallbackContext.getBLERequestHandler().addRequest(
	                		SendKeyExchangeOnlyWriteResponse.getInstance(mUseCaseContext), ResquestKESM4Parameter, mResponseKESM4Blocks);
	                
	                // start to execute 
	                mCallbackContext.getBLERequestHandler().startHandleRequest();
                }
                else
                {
                    Debug.printD(TAG, "KES M4 done!");
                }
	            
                                              
            }

        }

        
    }
    
    
    private class ResponseKESM5Blocks implements ResponseCallbackWithData
    {
        /**
         * This method is called after the response is received.
         * No use in this response 
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
           // No functionality
        }

        @Override
        public void onRequestCompleted(SafetyBoolean result, ResponsePack pack)
        {
         // Result byte value
            byte isSuccess = result.getByte();
            
            if (isSuccess != SAFETY_TRUE)
            {               
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else
            {
                AttributeChangeNotification response = 
                        (AttributeChangeNotification)pack.getResponse();
            
                SafetyByteArray sbData = response.getData();
	            byte[] bData = sbData.getByteArray();
	            ByteBuffer bbData = ByteBuffer.wrap(bData);
	            
	            // Get OpCode
	            bbData.rewind();
	            bbData.order(ByteOrder.LITTLE_ENDIAN);
	            int iOpCode = bbData.get();
	            int mOffset = bbData.getShort() & TWO_BYTES_OPERAND;
	            int mlength = bbData.get();
	            Debug.printD(TAG, " KESM5 OpCode = : " + iOpCode);
	            
	            byte[] temp = new byte[mlength];
	            bbData.get(temp, 0, mlength);
	            
	            Debug.printD(TAG, "111111111111111111111111111111111111111111111");
	            Debug.printD(TAG,
	                    "KESM5_DATAResponse OpCode = " + iOpCode);
	            Debug.printD(TAG,
	                    "KESM5_DATAResponse M5_Offset = " + mOffset);
	            Debug.printD(TAG,
	                    "KESM5_DATAResponse M5_Length = " + mlength);

	            Debug.printD(TAG, "111111111111111111111111111111111111111111111");
	            
	            mReceivedData.append(temp, 0, temp.length);

	            if ((M5_FINAL_BLOCK_LENGTH == mlength)
	                    && ((M5_TOTAL_LENGTH - M5_FINAL_BLOCK_LENGTH) == mOffset))
	            {

	                Debug.printD(TAG, "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY");
	                Debug.printD(TAG, "M3_data = ");
	                Debug.dumpPayload(TAG, mReceivedData.toByteArray());
	                Debug.printD(TAG, "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY");

	                mSentOutData = ByteBuffer.wrap(KeyExchagneJNI
	                        .Kes_step3(mReceivedData.toByteArray()));
	                mReceivedData.clear();
 
	                byte[] data = getRemainingData(mSentOutData, ControlPointConstant.KESOpCode.M6_DATA);
	                BLERequestParameter ResquestKESM6Parameter = new BLERequestParameter();
	                ResquestKESM6Parameter.setData(new SafetyByteArray(data,CRCTool.generateCRC16(data)));
	                // add ReadDeviceInfo request in list
	                mCallbackContext.getBLERequestHandler().addRequest(
	                		SendKeyExchangeOnlyWriteResponse.getInstance(mUseCaseContext), ResquestKESM6Parameter, mResponseKESM6Blocks);
	                
	                // start to execute 
	                mCallbackContext.getBLERequestHandler().startHandleRequest();
	            }
	            else
	            {
	                // Apply to the coding standard
	            }

                                              
            }

        }
  
    }
    
    
    private class ResponseKESM6Blocks implements ResponseCallbackWithData
    {
        /**
         * This method is called after the response is received.
         * No use in this response 
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
           // No functionality
        }

        @Override
        public void onRequestCompleted(SafetyBoolean result, ResponsePack pack)
        {
         // Result byte value
            byte isSuccess = result.getByte();
            
            if (isSuccess != SAFETY_TRUE)
            {               
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else
            {
            
                int remainingLength = mSentOutData.remaining();
                
                if (0 < remainingLength)
                {
              
                    byte[] data = getRemainingData(mSentOutData, ControlPointConstant.KESOpCode.M6_DATA);
	                BLERequestParameter ResquestKESM6Parameter = new BLERequestParameter();
	                ResquestKESM6Parameter.setData(new SafetyByteArray(data,CRCTool.generateCRC16(data)));
	                // add ReadDeviceInfo request in list
	                mCallbackContext.getBLERequestHandler().addRequest(
	                		SendKeyExchangeOnlyWriteResponse.getInstance(mUseCaseContext), ResquestKESM6Parameter, mResponseKESM6Blocks);
	                
	                // start to execute 
	                mCallbackContext.getBLERequestHandler().startHandleRequest();
                }
                else
                {
                	Debug.printD(TAG, "KES M6 done!");
                    int i = 0;
                    ByteBuffer mKeyB = ByteBuffer.wrap(KeyExchagneJNI.Kes_done());
                    byte[] temp = new byte[KEY_LENGTH];
                    mKeyB.get(temp); 
                    GlobalTools.MPR.setKey(temp);
                    Debug.printD(TAG, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
                    for (i = 0; i < KEY_LENGTH; i++)
                    {
                        Debug.printD(TAG, "Key = " + temp[i]);
                    }
                    Debug.printD(TAG, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
                    Debug.printD(TAG, "Transfer finished.");
                }
	            

                
                                              
            }

        }

        
    }
    
    /**
     * 
     * This method gets the remaining Key Exchange M2,M4,M6 data block.
     * 
     * @param byteBuffer[in] Command relevant data (M2/M4/M6 data block)
     *            Range: a valid object of ByteBuffer
     *            Unit: ByteBuffer
     *            Scaling: 1
     * @param opCode[in] Command relevant data (OP code)
     *            Range: refer to Communication Spec..
     *            Unit: byte
     *            Scaling: 1
     * 
     * @return void [out]
     * 
     */
    private byte[] getRemainingData(ByteBuffer byteBuffer, byte opCode)
    {
        final int LENGTH_OF_BLOCK_DATA = 16;
        ByteArrayBuffer result = new ByteArrayBuffer(0);
        byte[] data = null;
        byte[] bytesOfOffset = BLEController.parseInt16(byteBuffer.position());
        int lengthToTransfer = byteBuffer.remaining();

        if (lengthToTransfer > LENGTH_OF_BLOCK_DATA)
        {
            lengthToTransfer = LENGTH_OF_BLOCK_DATA;
        }
        else
        {
            // Apply to the coding standard
        }

        data = new byte[lengthToTransfer];

        byteBuffer.get(data, 0, lengthToTransfer);

        result.append(opCode);
        result.append(bytesOfOffset, 0, bytesOfOffset.length);
        result.append(lengthToTransfer);
        result.append(data, 0, data.length);

        return result.toByteArray();
    }
    
    
    private class ResponseKESVersion implements ResponseCallbackWithData
    {
        /**
         * This method is called after the response is received.
         * No use in this response 
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
           // No functionality
        }

        @Override
        public void onRequestCompleted(SafetyBoolean result, ResponsePack pack)
        {
        	Debug.printD(TAG, "ResponseKESVersion");
            // Result byte value
            byte isSuccess = result.getByte();
            
            if (isSuccess != SAFETY_TRUE)
            {               
                setupReturnResult(SafetyBoolean.FALSE);
            }
            else
            {
                AttributeChangeNotification response = 
                        (AttributeChangeNotification)pack.getResponse();
            
                SafetyByteArray sbData = response.getData();
	            byte[] bData = sbData.getByteArray();
	            ByteBuffer bbData = ByteBuffer.wrap(bData);
	            
	            // Get OpCode
	            bbData.rewind();
	            bbData.order(ByteOrder.LITTLE_ENDIAN);
	            int iOpCode = bbData.get();
                int iKESMajorVersion = bbData.get();
                int iKESMinorVersion = bbData.get();
	            
	            Debug.printD(TAG, " KESVersionResponse OpCode = : " + iOpCode);
                Debug.printD(TAG, "KESVersionResponse MajorVersion = " + iKESMajorVersion);
                Debug.printD(TAG, "KESVersionResponse MinorVersion = " + iKESMinorVersion);

                byte[] data = { ControlPointConstant.KESOpCode.STEP1 };

                BLERequestParameter ResquestKESStep1Parameter = new BLERequestParameter();
                ResquestKESStep1Parameter.setData(new SafetyByteArray(data,CRCTool.generateCRC16(data)));
                // add ReadDeviceInfo request in list
                mCallbackContext.getBLERequestHandler().addRequest(
                		SendKeyExchangeCP.getInstance(mUseCaseContext), ResquestKESStep1Parameter, mResponseKESM1Blocks);
                               
            }

        }

        
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
        // Set wait state to leave this activity 

	    mUseCaseContext.unregisterReceiver(mKESReceiver);
    }
    
    
    
    private class DisconnectDone implements ResponseCallback
    {
        /**
         * This method removes the Pin code from memory.It returns the failure 
         * of BLE_UC01 process via callback function.
         * 
         * 
         * @param result: the result of Disconnect
         *            Range: a valid object of SafetyBoolean
         *            Unit: SafetyBoolean
         *            Scaling: 1
         *  
         * @return void [out]             
         *            
         * @see mCallback          
         */
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
        	Debug.printD(TAG, " [Disconnect Done...........  ] ");
            GlobalTools.MPR.setPinCode(null);
            setupReturnResult(SafetyBoolean.FALSE);  
        }
    }
    
    
    /**
     * This method disconnects BLE-Device.
     * 
     * @param N/A
     * 
     * @return void [out] 
     * 
     * @see mDisconnectDone
     * 
     */
    private void disconnect()
    {
        Debug.printD(TAG, " [UC01 Failed...........  ] ");

        mCallbackContext.getBLERequestHandler().clearRequest();
        
        Debug.printD(TAG, " [Disconnect ...... ] ");
        mCallbackContext.getBLERequestHandler().addRequest(
                Disconnect.getInstance(mUseCaseContext), null, mDisconnectDone); 
        
        mCallbackContext.getBLERequestHandler().startHandleRequest();
    }
    


    /**
     * This method is called after the ConnectStateChanged notification is received. 
     * If it is disconnected, it returns failed via callback function. 
     *
     * @param isConnected [in] the BLE-Device is disconnected or not.      
     *            Range: a valid object of SafetyBoolean
     *            Unit: SafetyBoolean
     *            Scaling: 1  
     *            
     * @return void [out]
     * @see mCallback
     * 
     */
    public void onConnectionStateChanged(int state)
    {
        
    	boolean isNotConnected = (( CommsConstant.BtState.DISCONNECTED == state ) 
    			|| ( CommsConstant.BtState.CONNECTIONLOST == state ));
    	
      
        if ( isNotConnected)            
        {
        	Debug.printD(TAG, "[disconnect ] ");
        	setupReturnResult(SafetyBoolean.FALSE);              
        }
        else
        {
            // Apply to the coding standard
        }
    }
    
    /**
     * This method is called after RequestTimeout. It calls disconnect-function. 
     *
     * @param request [in] the request     
     *            Range: a valid object of IBLERequest
     *            Unit: IBLERequest
     *            Scaling: 1  
     * @param parameter [in] the parameter of request
     *            Range: a valid object of BLERequestParameter
     *            Unit: BLERequestParameter
     *            Scaling: 1 
     *                      
     * @return void [out]
     * 
     * 
     */
    public void onRequestTimeout(IBLERequest request,
            BLERequestParameter parameter)
    {       
        // CMD timeout, then disconnect BT device      
        disconnect(); 
        Debug.printD(TAG, request.getClass().getSimpleName() + " timeout!");
    }

}

/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */