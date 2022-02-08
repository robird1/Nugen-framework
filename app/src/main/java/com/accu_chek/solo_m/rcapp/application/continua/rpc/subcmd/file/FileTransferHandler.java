/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.DeleteFile
 * Brief: 
 *
 * Create Date: 2015/7/1
 * $Revision: 23125 $
 * $Author: kevenwu $
 * $Id: FileTransferHandler.java 23125 2015-11-03 11:16:42Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.rpc.subcmd.file;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.http.util.ByteArrayBuffer;

import android.os.Environment;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandController;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaConstants.KeySelectorType;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCArgumentType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCErrorResponse;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.RPCConstants.RPCEventType;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCDataArguments;
import com.accu_chek.solo_m.rcapp.application.continua.rpc.data.RPCParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.settinginterface.UserSettingsXml;

public class FileTransferHandler
{    
	/**
	 * The file path of received data which comes from RPC command.
	 */
	public static final String FILE_PATH = Environment.getExternalStorageDirectory()
			.getAbsolutePath() + "/nugen/RPC_File/";
    /**
     * The singleton instance of FileTransferHandler.
     */
    private static final FileTransferHandler mInstance = new FileTransferHandler();
    
    /**
     * The size of APDU.
     */
    private static final int APDU_SIZE = 498;
    
    /**
     * The header size in file data event report.
     */
    private static final int INDEX_OF_FILE_DATA_LENGTH = 16;
    
    /**
     * The file which stores the data from Continua Manager.
     */
    private File mReceivedFile = null;
    
    /**
     * The signature data of received file.
     */
    private byte[] mSignature = null;
    
    /**
     * The flag of transferring procedure started, it's used to abort the procedure.
     */
    private AtomicBoolean mIsProcedureStarted = new AtomicBoolean(false);
    
    /**
     * The handler which handle the file event report timeout.
     */
    private Handler mTimeoutHandler = null;
    
    /**
     * Return the singleton instance of FileTransferHandler.
     * 
     * see mInstance [out]
     * 
     * return FileTransferHandler [out]: The instance of FileTransferHandler.
     *        Range: Valid object of FileTransferHandler.
     *        Unit: FileTransferHandler.
     *        Scaling: 1.
     */
    public static FileTransferHandler getInstance()
    {
        return mInstance;
    }
    
    /**
     * Private constructor to keep only one instance of this class.
     */
    private FileTransferHandler()
    {        
    	// Do nothing.
    }
    
    /**
     * Start transferring the file. 
     * If the file is larger than APDU size, it is divided to meet the APDU size and then transferred.
     * 
     * @param commandSet :The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     * @param file : The target file which is required to transfer.
     *        Range: Valid object of File.
     *        Unit: File.
     *        Scaling: 1.
     *        
     * return void [out]: None.        
     */
    public synchronized void startTransfer(ContinuaCommandSet commandSet, File file)
    {
        SafetyByteArray response = null;
        BufferedReader reader = null;
        
        try 
        {
			reader = new BufferedReader(new FileReader(file));
			
			mIsProcedureStarted.set(true);
			
			do
	        {
	        	response = retrieveAPDUDataFromFile(reader);
	        	commandSet.getController().setSegmentDataOfRPC(response);
	        	
	        } while (mIsProcedureStarted.get());
		} 
        catch (FileNotFoundException e) 
        {
            response = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_APPLICATION);
            commandSet.getController().setSegmentDataOfRPC(response);
			e.printStackTrace();
		}
        catch (IOException e)
        {
            response = RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_HARDWARE);
            commandSet.getController().setSegmentDataOfRPC(response);
            e.printStackTrace();
        }
        finally
        {
            mIsProcedureStarted.set(false);
            
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
    }
    
    /**
     * Retrieve the data from the file reader according to the position to which data has been transferred.
     * Return the RPC command response which contains file data. 
	 * If some error occurs return error response.
     * 
     * @param reader : BufferedReader contains the target file.
     * 		  Range: Valid object of BufferedReader.
     * 		  Unit: BufferedReader.
     * 		  Scaling: 1.
     * 
     * return SafetyByteArray [out]: The response contains the file data or error response.
     * 		  Range: Valid object of SafetyByteArray.
     * 		  Unit: SafetyByteArray.
     * 		  Scaling: 1.
     * 
     * throws IOException if this file reader is closed or some other I/O error occurs.
     */
    protected SafetyByteArray retrieveAPDUDataFromFile(BufferedReader reader) throws IOException
    {
    	SafetyByteArray response = null;
    	
        RPCDataArguments argument = new RPCDataArguments();
        ByteArrayBuffer buffer = new ByteArrayBuffer(0);
        
    	char[] tempData = new char[APDU_SIZE];    	

    	int result = reader.read(tempData);
    	
		buffer.append(tempData, 0, tempData.length);
    	
    	if (APDU_SIZE != result)
    	{
    		mIsProcedureStarted.set(false);
    	}
    	else
    	{
    		// Apply to coding standard.
    	}
        
        argument.setType(RPCArgumentType.RPC_ARG_TYPE_UINT8_ARRAY);
        argument.setLength(new SafetyNumber<Integer>(buffer.length(), -buffer.length()));
        argument.setValue(new SafetyByteArray(buffer.toByteArray(), 
        		CRCTool.generateCRC16(buffer.toByteArray())));
        
        response = RPCParseUtils.generateRPCResponse(
                RPCEventType.MDC_NOTI_RPC_COMMAND_RESPONSE, argument);
    	
    	return response;
    }
    
    /**
     * Abort the data transfer procedure.
     * Set flag mIsProcedureStarted to false.
     * 
     * see mIsProcedureStarted [in]
     * 
     * return void [out]: None.
     */
    public void abortTransfer()
    {
    	mIsProcedureStarted.set(false);
    }
    
    /**
     * Initialize the received file name and path.
     * If the write mode is overwriten, the original file is copied and add "temp" at the end of file name.
     * 
     * @param file : The target file of received data.
     *        Range: Valid object of File.
     *        Unit: File.
     *        Scaling: 1.
     * @param mode : The write mode of target file.
     * 		  Range: WriteFile.RPC_WRITE_FILE_NEW,
     * 			     WriteFile.RPC_WRITE_FILE_OVERWRITE,
     * 				 WriteFile.RPC_WRITE_FILE_APPEND.
     * 		  Unit: Integer.
     * 		  Scaling: 1.
     * @param signature : The 
     *        
     * see mReceivedFile [in]        
     *        
     * return void [out]: None.        
     */
    public void initReceiveFile(File file, int mode, byte[] signature)
    {	        
        new File(FILE_PATH).mkdirs();
        
        if (WriteFile.RPC_WRITE_FILE_OVERWRITE == mode)
        {        	
        	file.renameTo(new File(file.getAbsolutePath() + "temp"));
        	file = new File(file.getAbsolutePath());
        }
        else
        {
            // Apply to coding standard.
        }
        
        mTimeoutHandler = new Handler(Looper.getMainLooper())
        {
            /**
             * Send the command timeout response to Agent.
             * 
             * @param msg : The message contain the ContinuaCommandController and indicate timeout.
             *        Range: Valid object of Message.
             *        Unit: Message.
             *        Scaling: 1.
             *        
             * return void [out]: None.        
             */
            @Override
            public void handleMessage(Message msg)
            {
                ((ContinuaCommandController) msg.obj).setSegmentDataOfRPC(
                        RPCParseUtils.generateErrorResponse(
                                RPCErrorResponse.RPC_ERR_COMMAND_TIMEOUT_EXCEEDED));
            }             
        };
        
        mReceivedFile = file;
        mSignature = Arrays.copyOf(signature, signature.length);
    }
    
    /**
     * Append the received data into the target file. 
     * And delete the temporary file after the write file command completed.
     * 
     * @param commandSet : The command set contains the received data from Continua Manager.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     *        
     * return void [out]: None.
     */
    public void writeReceivedData(ContinuaCommandSet commandSet)
    {   
        final int TIMEOUT = 15000;
        final byte[] EVENT_REPORT_CONFIRM = {0x00, 0x00, (byte) 0xFF, (byte) 0xFF, 
                (byte) 0xFF, (byte) 0xFF, (byte) 0xF0, 0x05, 0x00, 0x00};
        
        Message timeoutMessage = Message.obtain(mTimeoutHandler, 0);
        ByteBuffer buffer = ByteBuffer.wrap(commandSet.getDataOfCommand().getByteArray());
        int capacity = buffer.capacity();
        FileWriter writer = null;
        
        mTimeoutHandler.removeMessages(0, null);
        
        try
        {
            byte[] fileData = null;
            
            writer = new FileWriter(mReceivedFile, true);
            
            buffer.position(INDEX_OF_FILE_DATA_LENGTH);            
            
            fileData = new byte[buffer.getShort()];            
            buffer.get(fileData);
            
            for (byte each : fileData)
            {
                writer.append((char) each);
            }
            
            commandSet.getController().setSegmentDataOfRPC(
                    ParseUtils.appendCRC(EVENT_REPORT_CONFIRM));
            
            timeoutMessage.obj = commandSet.getController();
            mTimeoutHandler.sendMessageDelayed(timeoutMessage, TIMEOUT);
            
            if (APDU_SIZE != capacity)
            {
                boolean isCheckPass = verifySignature(commandSet);
                
                SafetyByteArray response = RPCParseUtils.generateErrorResponse(
                        RPCErrorResponse.RPC_ERR_SECURITY_ERROR);
                
                if (isCheckPass)
                {
                    new File(mReceivedFile.getAbsolutePath() + "temp").delete();
                    
                    if (mReceivedFile.getName().contains(UserSettingsXml.NAME_USER_SETTINGS))
                    {
                        UserSettingsXml.loadUserSettings();
                    }
                    else
                    {
                        // Apply to coding standard.
                    }
                    
                    response = RPCParseUtils.generateErrorResponse(
                            RPCErrorResponse.RPC_ERR_NO_ERRORS);
                }
                else
                {
                    // Apply to coding standard.
                }
                
                mTimeoutHandler.removeMessages(0);
                commandSet.getController().setSegmentDataOfRPC(response);                
            }
            else
            {
                // Apply to coding standard.
            }
        }
        catch (IOException e)
        {
            commandSet.getController().setSegmentDataOfRPC(
                    RPCParseUtils.generateErrorResponse(RPCErrorResponse.RPC_ERR_HARDWARE));
            e.printStackTrace();
        }
        finally
        {
            try
            {
                if (null != writer)
                {
                    writer.close();
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
    }
    
    /**
     * Verify the signature of received data to check the status of the write file action.
     * 
     * @param commandSet : The command set contains the received data from Continua Manager.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     * 
     * return boolean [out]: True if the verification is successful, otherwise false.
     *        Range: True or False.
     *        Unit: boolean.
     *        Scaling: 1.
     */
    protected boolean verifySignature(ContinuaCommandSet commandSet)
    {
        boolean result = false;
        DataInputStream reader = null;
        byte[] fileData = new byte[(int) mReceivedFile.length()];
        
        try
        {               
            reader = new DataInputStream(new FileInputStream(mReceivedFile));
            reader.readFully(fileData);
            
            result = commandSet.getController().verifySignedSignature(
                    KeySelectorType.ROCHE_IM, fileData, mSignature);
        }
        catch (Exception e)
        {
            // If exception appear, the result is default value FALSE.
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
        
        return result;
    }
}
