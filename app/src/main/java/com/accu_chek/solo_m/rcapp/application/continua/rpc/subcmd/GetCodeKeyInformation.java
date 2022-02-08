/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.GetCodeKeyInformation
 * Brief: 
 *
 * Create Date: 2015/7/14
 * $Revision: 21737 $
 * $Author: kevenwu $
 * $Id: GetCodeKeyInformation.java 21737 2015-10-16 10:48:49Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd;

import java.nio.ByteBuffer;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import org.apache.http.util.ByteArrayBuffer;

import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.IBgmConstant;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.IRPCCommandHandler;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.IBGMControl;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.IMeInformationListener;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class GetCodeKeyInformation implements IRPCCommandHandler
{
    /**
     * The time format which is used in measurement engine.
     */
    private final SimpleDateFormat FORMAT = new SimpleDateFormat("yyyyMMdd");
    
    /**
     * The fixed century field in configuration matrix.
     */
    private final String CENTURY = "20";
    
    /**
     * The parameter of parsing integer to hex.
     */
    private final int HEX = 16;
    
    /**
     * The timeout of communication with measurement engine.
     */
    private final int TIMEOUT = 10000;
    
    /**
     * The data argument of RPC command response.
     */
    private RPCDataArguments mArgument = new RPCDataArguments();
    
    /**
     * The data which contains completed command response.
     */
    private SafetyByteArray mResponse = new SafetyByteArray();
    
    /**
     * The callback function of measurement engine information.
     * It handle the code key information here.
     */
    private IMeInformationListener mMeInfoListener = new MeInformationHandler();
    
    private class MeInformationHandler extends IMeInformationListener.Stub
    {
        /**
         * Convert the code key information to Continua defined structure.
         *
         * @param number : The number of code key information.
         *        Range: Valid object of SafetyByteArray.
         *        Unit: SafetyByteArray.
         *        Scaling: 1.
         * @param date : The date information of code key.
         *        Range: Valid object of SafetyByteArray.
         *        Unit: SafetyByteArray.
         *        Scaling: 1.
         * @param status : The status information of code key.
         *        Range: Valid object of SafetyByteArray.
         *        Unit: SafetyByteArray.
         *        Scaling: 1.
         * @param field : The internal option of code key.
         *        Range: Valid object of SafetyByteArray.
         *        Unit: SafetyByteArray.
         *        Scaling: 1.
         *        
         * see mArgument [in]
         * see mResponse [in]        
         *        
         * throws RemoteException if communication failed with measurement engine.
         */
        @Override
        public void onCodeKeyInformation(SafetyByteArray number,
                SafetyByteArray date, SafetyByteArray status,
                SafetyByteArray field) throws RemoteException
        {
            ByteArrayBuffer buffer = new ByteArrayBuffer(0);
            
            byte[] numberInBytes = ParseUtils.parseInt16(
                    Integer.parseInt(new String(number.getByteArray()), HEX));
            int statusValue = Integer.parseInt(new String(status.getByteArray()), HEX);
            int internalOption = 0;
            byte[] dateInBytes = null;
            
            Calendar calendar = Calendar.getInstance();            
            ByteBuffer bufferOption = ByteBuffer.wrap(field.getByteArray());
            
            internalOption = Integer.parseInt(String.valueOf(bufferOption.get()) 
                    + String.valueOf(bufferOption.get()));
            
            try
            {
                String fullDate = CENTURY + new String(date.getByteArray());
                
                calendar.setTime(FORMAT.parse(fullDate));
                
                dateInBytes = ParseUtils.parseAbsoluteTime(calendar.getTimeInMillis());
                
                buffer.append(numberInBytes, 0, numberInBytes.length);
                buffer.append(dateInBytes, 0, dateInBytes.length);
                buffer.append(statusValue);
                buffer.append(internalOption);
                
                mArgument.setType(RPCArgumentType.RPC_ARG_TYPE_CODEKEY);
                mArgument.setLength(new SafetyNumber<Integer>(buffer.length(), 
                        -buffer.length()));
                mArgument.setValue(new SafetyByteArray(buffer.toByteArray(), 
                        CRCTool.generateCRC16(buffer.toByteArray())));
                
                mResponse = RPCParseUtils.generateRPCResponse(
                        RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, 
                        mArgument);
            }
            catch (ParseException e)
            {
                e.printStackTrace();
            }
            finally
            {
                // Do nothing.
            }
            
            synchronized (mArgument)
            {
                mArgument.notifyAll();
            }
        }

        @Override
        public void onStripCounter(SafetyByteArray counter)
                throws RemoteException
        {
            // unused function.
        }

        @Override
        public void onError() throws RemoteException
        {
            // Do nothing. the original response is application error.
        }

        @Override
        public void onSuccess(SafetyString key, SafetyString result) throws RemoteException
        {
            // unused function.
        }
    }
    
    /**
     * Get code key information from measurement engine and return to Agent.
     * If the communication failed with measurement engine, return application error to Agent.
     *
     * @param commandSet :The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     * @param invocation : The data arguments of request RPC command.
     *        Range: Valid object of RPCCommandInvocation.
     *        Unit: RPCCommandInvocation.
     *        Scaling: 1.        
     *        
     * return void [out]: None.        
     */
    @Override
    public void handle(final ContinuaCommandSet commandSet, 
            RPCCommandInvocation invocation)
    {
        IBGMControl controller = CustJavaFrameworkManager.getBGMControlService(
                commandSet.getController().getContext());
        
        try
        {            
            // Assign the error response, if timeout it will be the response.
            mResponse = RPCParseUtils.generateErrorResponse(
                    RPCErrorResponse.RPC_ERR_APPLICATION);
            
            CommonUtils.objectCheck(controller);
            
            controller.getMeInformation(IBgmConstant.CHECKCODEKEY, mMeInfoListener);
            
            synchronized (mArgument)
            {
                mArgument.wait(TIMEOUT);
            }
        }
        catch (RemoteException e)
        {
            // The response is application error.
            e.printStackTrace();
        }
        catch (InterruptedException e)
        {
            // Timeout occur or code key information has been read.
            e.printStackTrace();
        }
        finally
        {
            commandSet.getController().setSegmentDataOfRPC(mResponse);
        }
    }
}
