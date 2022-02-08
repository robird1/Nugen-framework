/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController
 * Brief: 
 *
 * Create Date: 2015/3/18
 * $Revision: 22494 $
 * $Author: kevenwu $
 * $Id: ContinuaCommandController.java 22494 2015-10-26 03:35:47Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.commandcontorller;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import org.apache.http.util.ByteArrayBuffer;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.continua.MindtreeService;
import com.accu_chek.solo_m.rcapp.application.continua.MindtreeService.OnJNIListener;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.ErrorCode;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.KeySelectorType;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.file.FileTransferHandler;
import com.accu_chek.solo_m.rcapp.application.continua.typehandler.IContinuaCommandHandler.ContinuaCommand;
import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;

/**
 * The controller of Continua for collect all necessary data from device.
 */
public class ContinuaCommandController implements OnJNIListener
{    
    /**
     * The context of Application. It's used to access the database.
     */
    private final Context mContext;
    
    /**
     * The instance of Continua Agent.
     */
    private final MindtreeService mContinuaAgent;
    
    /**
     * Constructs a new ContinuaCommandController and sets up the JNI of ContinuaCommandController.
     * 
     * @param context : The application context. Context is provided by Android.
     *        Range: Valid Context object in Android.
     *        Unit: Context.
     *        Scaling: 1.
     *        
     * see mContext [out]
     * see mContinuaAgent [out]
     */
    public ContinuaCommandController(Context context)
    {
        mContext = context;
        mContinuaAgent = new MindtreeService(context, this);
    }
    
    /**
     * Return the application context.
     *
     * see mContext [in]
     *
     * return context [out]: The application context. Context is provided by Android.
     *        Range: Valid Context object in Android.
     *        Unit: Context.
     *        Scaling: 1.
     */
    public Context getContext()
    {
        return mContext;
    }
    
    /**
     * Initiate the Continua Agent module.
     *
     * see mContinuaAgent [in]
     *
     * return void [out]: None
     */
    public void startContinua()
    {
        mContinuaAgent.initContinua();
    }
    
    /**
     * Stop the Continua Agent module.
     *
     * see mContinuaAgent [in]
     *
     * return void [out]: None
     */
    public void stopContinua()
    {
        mContinuaAgent.closeContinua();
        mContinuaAgent.release();
    }
    
    /**
     * Delegate the segment data to Continua Agent.
     *
     * @param type : The command of this segment data.
     *        Range: Valid object of ContinuaCommand.
     *        Unit: ContinuaCommand.
     *        Scaling: 1. 
     * @param segment : The target segment data.
     *        Range: Valid object of each segment data definition.
     *        Unit: SafetyByteArray.
     *        Scaling: 1.
     *
     * see mContinuaAgent [in]
     *
     * return void [out]: None
     */
    public void setSegmentDataToAgent(ContinuaCommand type, SafetyByteArray segment)
    {
        mContinuaAgent.setIEEEObjectData(type.getCommandType(), segment.getByteArray());
    }
    
    /**
     * Delegate the segment data of 10417 to Continua Agent.
     *
     * @param type : The command of this segment data.
     *        Range: Valid object of ContinuaCommand.
     *        Unit: ContinuaCommand.
     *        Scaling: 1. 
     * @param segment : The target segment data.
     *        Range: Valid object of each segment data definition.
     *        Unit: SafetyByteArray.
     *        Scaling: 1.
     *
     * see mContinuaAgent [in]
     *
     * return void [out]: None
     */
    public void setSegmentDataOf10417(ContinuaCommand type, SafetyByteArray segment)
    {
        mContinuaAgent.setContinuaDataOf10417(type.getCommandType(), segment.getByteArray());
    }
    
    /**
     * Delegate the segment data of 10419 to Continua Agent.
     *
     * @param type : The command of this segment data.
     *        Range: Valid object of ContinuaCommand.
     *        Unit: ContinuaCommand.
     *        Scaling: 1. 
     * @param segment : The target segment data.
     *        Range: Valid object of each segment data definition.
     *        Unit: SafetyByteArray.
     *        Scaling: 1.
     *
     * see mContinuaAgent [in]
     *
     * return void [out]: None
     */
    public void setSegmentDataOf10419(ContinuaCommand type, SafetyByteArray segment)
    {
        mContinuaAgent.setContinuaDataOf10419(type.getCommandType(), segment.getByteArray());
    }
    
    /**
     * Delegate the segment data of RPC to Continua Agent.
     *
     * @param segment : The target segment data.
     *        Range: Valid object of each segment data definition.
     *        Unit: SafetyByteArray.
     *        Scaling: 1.
     *
     * see mContinuaAgent [in]
     *
     * return void [out]: None
     */
    public void setSegmentDataOfRPC(SafetyByteArray segment)
    {
        mContinuaAgent.setContinuaDataOfRPC(segment.getByteArray());
    }
    
    /**
     * Handle the request from Continua manager. The request command will be
     * recognized and transferred to relevant command parser.
     *
     * @param type : The request command from Continua manager.
     *        Range: Refer to the definition of ContinuaRequestType.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param configId : It is used to recognize which type of configuration
     * should be used.
     *        Range: Refer to the definition of GlucoseConfigId.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param segmentId : The segmentId of the configuration.
     *        Range: Refer to the definition of GlucoseSegmentId.
     *        Unit: Integer.
     *        Scaling: 1.
     *   
     * return void [out]: None
     */
    @Override
    public void onRequestReceived(int type, int configId, int segmentId)
    {
        try
        {            
            ContinuaCommand.getRequestCommand(type).getHandler().handleCommand(
                    new ContinuaCommandSet(this, type, configId, segmentId));
        }
        catch (ArgumentErrorException e)
        {
            byte[] error = ParseUtils.parseInt16(ErrorCode.COMMAND_NOT_SUPPORTED);
            mContinuaAgent.setIEEEObjectData(type, error);
            e.printStackTrace();
        }
        finally
        {
            // Do nothing.
        }
    }

    /**
     * Handle the request from Continua manager. The request command will be
     * recognized and transferred to relevant command parser.
     *
     * @param type : The request command from Continua manager.
     *        Range: Refer to the definition of ContinuaRequestType.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param configId : It is used to recognize which type of configuration
     * should be used.
     *        Range: Refer to the definition of GlucoseConfigId.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param segmentId : The segmentId of the configuration.
     *        Range: Refer to the definition of GlucoseSegmentId.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param data : The data of request command.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.        
     *   
     * return void [out]: None
     */
    @Override
    public void onRequestWithDataReceived(int type, int configId,
            int segmentId, byte[] data)
    {
        try
        {            
            ContinuaCommand.getRequestCommand(type).getHandler().handleCommand(
                    new ContinuaCommandSet(this, type, configId, segmentId, data));
        }
        catch (ArgumentErrorException e)
        {
            byte[] error = ParseUtils.parseInt16(ErrorCode.COMMAND_NOT_SUPPORTED);
            mContinuaAgent.setIEEEObjectData(type, error);
            e.printStackTrace();
        }
        finally
        {
            // Do nothing.
        }
    }

    /**
     * Handle the RPC from Continua manager. The request command will be
     * recognized and transferred to relevant command parser.
     * 
     * @param data : The data of request command.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.        
     *   
     * return void [out]: None
     */
    @Override
    public void onRPCCommandReceived(byte[] data)
    {        
        ContinuaCommand.RPC_COMMAND.getHandler().handleCommand(
                new ContinuaCommandSet(this, 0, 0, 0, data));
    }

    /**
     * Retrieve the authentication key from specified file and return to Agent.
     * 
     * @param keyType : The value of key type.
     *        Range: Refer to the definition of AuthenticationKey.
     *        Unit: Integer.
     *        Scaling: 1.
     * 
     * return byte[] [out]: The data which contains the authentication key.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.
     */    
    @Override
    public byte[] getRPCKey(int keyType)
    {
        final int HEX = 16;
        
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        BufferedReader reader = null;
        File file = null;        
        
        switch (keyType)
        {
        case KeySelectorType.ROCHE_IM:
            file = new File(FileTransferHandler.FILE_PATH + ContinuaConstants.NAME_IM_KEY);
            break;
        case KeySelectorType.ROCHE_INTERNAL:
            file = new File(FileTransferHandler.FILE_PATH + ContinuaConstants.NAME_INTERNAL_KEY);
            break;
        default :
            // Do nothing.
            break;
        }
        
        try
        {   
            String content = "";
            
            reader = new BufferedReader(new FileReader(file));
            content = reader.readLine();
            
            if (null == content)
            {
                content = "";
            }
            else
            {
                // Apply to coding standard.
            }
            
            for (String each : content.split(","))
            {
                buffer.append(Integer.valueOf(each, HEX));
            }
        }
        catch (FileNotFoundException e)
        {
            e.printStackTrace();
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
        finally
        {
            try
            {
                if (null != reader)
                {
                    reader.close();
                }
                else
                {
                    // Apply to coding standard.
                }
            }
            catch (IOException e)
            {
                e.printStackTrace();
            }
            finally
            {
                // Apply to coding standard.
            }
        }
        
        return buffer.toByteArray();
    }

    /**
     * Delegate the data event report to FileTransferHandler to store the file.
     * If no error appear, return confirm event report to Agent.
     * If catch IOException, return hardware error to Agent.
     * 
     * @param data : The data contains the file in ADPU size.
     * 		  Range: Valid object of byte[].
     * 		  Unit: byte[].
     * 	  	  Scaling: 1.
     * 
     * return void [out]: None.
     */
	@Override
	public void onDataEventReport(byte[] data) 
	{
	    FileTransferHandler.getInstance().writeReceivedData(
	            new ContinuaCommandSet(this, 0, 0, 0, data));
	}
	
	/**
     * Delegate the received data and signed signature to JNI to verify the data.
     * If pass return true, otherwise return false.
     *
     * @param keyType : The key which is used to verify the signature.
     *        Range: Refer to the definition of KeySelectorType.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param original : The original data of transmission.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.
     * @param data : The signed signature data.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.     
     * 
     * return boolean [out] The result of verification of signed signature.
     *        Range: True or false.
     *        Unit: boolean.
     *        Scaling: 1.
     */
	public boolean verifySignedSignature(int keyType, byte[] original, byte[] data)
	{
	    return mContinuaAgent._VerifySignedSignature(keyType, original, data);
	}
}
