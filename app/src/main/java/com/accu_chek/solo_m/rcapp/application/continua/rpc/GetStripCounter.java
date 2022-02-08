/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.GetStripCounter
 * Brief: 
 *
 * Create Date: 2015/6/24
 * $Revision: 21737 $
 * $Author: kevenwu $
 * $Id: GetStripCounter.java 21737 2015-10-16 10:48:49Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc;

import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.IBGMControl;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.IBgmConstant;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.IMeInformationListener;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCCommandInvocation;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class GetStripCounter implements IRPCCommandHandler
{   
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
     */
    private IMeInformationListener mMeListener = new MeInformationHandler();
    
    private class MeInformationHandler extends IMeInformationListener.Stub
    {                
        /**
         * Put the input counter value to RPCDataArguments and generate a command response.
         *
         * @param counter : The strip counter value.
         *        Range: Valid object of SafetyByteArray.
         *        Unit: SafetyByteArray.
         *        Scaling: 1.
         * 
         * throws RemoteException if any exception appear.
         */
        @Override
        public void onStripCounter(SafetyByteArray counter) throws RemoteException
        {   
            String ascii = new String(counter.getByteArray());
            byte[] count = ParseUtils.parseInt16(Integer.parseInt(ascii, HEX));
            
            mArgument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT16);
            mArgument.setLength(new SafetyNumber<Integer>(count.length, -count.length));
            mArgument.setValue(new SafetyByteArray(count, CRCTool.generateCRC16(count)));
            
            mResponse = RPCParseUtils.generateRPCResponse(
                    RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, mArgument);
            
            synchronized (mArgument)
            {
                mArgument.notifyAll();
            }
        }
        
        @Override
        public void onError() throws RemoteException
        {
            // Do nothing.
        }
        
        @Override
        public void onCodeKeyInformation(SafetyByteArray number,
                SafetyByteArray date, SafetyByteArray status, SafetyByteArray field)
                throws RemoteException
        {
            // Do nothing.
        }

        @Override
        public void onSuccess(SafetyString key, SafetyString result) throws RemoteException
        {
            // Do nothing.
            
        }
    };
    
    /**
     * Retrieve the strip counter from measurement engine and return to Agent.
     * If the communication between BGM and this failed, return application error code to Agent.
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
    public void handle(ContinuaCommandSet commandSet, RPCCommandInvocation invocation)
    {        
        IBGMControl controller = CustJavaFrameworkManager.getBGMControlService(
                commandSet.getController().getContext());
        
        try
        {
            // Assign the error code, it will be response when timeout.
            mResponse = RPCParseUtils.generateErrorResponse(
                    RPCErrorResponse.RPC_ERR_APPLICATION);
            
            CommonUtils.objectCheck(controller);
            
            controller.getMeInformation(IBgmConstant.GETSTRIPCOUNTER, mMeListener);
            
            // wait for measurement engine response.
            synchronized (mArgument)
            {
                mArgument.wait(TIMEOUT);
            }            
        }
        catch (RemoteException e)
        {
            // The original response is application error.
            e.printStackTrace();
        }
        catch (InterruptedException e)
        {
            // The original response is application error.
            e.printStackTrace();
        }
        finally
        {
            commandSet.getController().setSegmentDataOfRPC(mResponse);
        }
    }
}
