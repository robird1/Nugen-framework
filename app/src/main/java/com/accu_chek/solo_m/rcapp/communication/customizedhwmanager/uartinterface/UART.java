/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.customizedhwmanager.uartinterface.UART
 * Brief: 
 * The class provides inner interface to UARTManager to call the function in JNI layer.
 * 
 * Create Date: 2015/1/19
 * $Revision: 20525 $
 * $Author: DWYang $
 * $Id: UART.java 20525 2015-10-01 11:16:30Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.communication.customizedhwmanager.uartinterface;

import java.util.Arrays;

import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;

/**
 * The class provides inner interface to UARTManager to call the function in JNI layer.
 */
final class UART extends IUART.Stub
{
    /**
     * not allowed read minimum range
     */
    public static final int NOT_ALLOWED_READ_BOTTOM_RANGE = 0;
    /**
     * not allowed read maximum range
     */
    public static final int NOT_ALLOWED_READ_TOP_RANGE = 513;
    /**
     * byte array's initial size
     */
    private final int ARRAY_INITIAL_SIZE = 0;
    
    /**
     * Open UART port.
     * 
     * @param port [in] port index in UARTPort.
     * Range: COMMSUART, BGMUART in UARTPort
     * Unit: int
     * Scaling: 1
     * 
     * return None 
     * 
     * throw IllegalStateException if port index is wrong or driver error occurs
     */
    public void open(int port) throws IllegalStateException
    {
        try
        {
            openUART(port);
        }
        catch (OperationFailException e)
        {
            e.printStackTrace();
            
            throw new IllegalStateException(e.getMessage());
        }
        finally
        {
            // Empty for static code analysis
        }
    }
    
    /**
     * Close UART port.
     * 
     * @param port [in] port index in UARTPort.
     * Range: COMMSUART, BGMUART in UARTPort
     * Unit: int
     * Scaling: 1
     * 
     * return None 
     * 
     * throw IllegalStateException if port index is wrong or driver error occurs
     */
    public void close(int port) throws IllegalStateException
    {
        try
        {
            closeUART(port);
        }
        catch (OperationFailException e)
        {
            e.printStackTrace();
            
            throw new IllegalStateException(e.getMessage());
        }
        finally
        {
            // Empty for static code analysis
        }
    }
    
    /**
     * Write data into UART port.
     * 
     * @param port [in] port index in UARTPort.
     * Range: COMMSUART, BGMUART in UARTPort
     * Unit: int
     * Scaling: 1
     * @param data [in] safety byte array that is sending data. For detail of this object, see SafetyByteArray.
     * byte array in SafetyByteArray:
     * Range: 0 ... unlimited
     * Unit: byte
     * Scaling: 1
     * 
     * return None 
     * 
     * throw IllegalStateException if port index is wrong or driver error occurs
     */
    @Override
    public void write(int port, SafetyByteArray data) throws IllegalStateException
    {
        try
        {
            writeUART(port, data.getByteArray());
        }
        catch (OperationFailException e)
        {
            e.printStackTrace();
            
            throw new IllegalStateException(e.getMessage());
        }
        finally
        {
            // Empty for static code analysis
        }
    }
    
    /**
     * Read data from UART port.
     * 
     * see ARRAY_INITIAL_SIZE [in] This global variable is referred for byte array's initial size.
     * see ALLOWED_READ_MIN_RANGE [in] This global variable is referred for allowed read minimum range.
     * see ALLOWED_READ_MAX_RANGE [in] This global variable is referred for allowed read maximum range.
     * 
     * @param port [in] port index in UARTPort.
     * Range: COMMSUART, BGMUART in UARTPort
     * Unit: int
     * Scaling: 1
     * @param byteLength [in] expected byte array size.
     * Range: 1 ... 512
     * Unit: byte
     * Scaling: 1
     * 
     * return SafetyByteArray that stores a byte array and is received from the port. 
     * If there is no any data from UART port, return a byte array with zero length in safety byte array. 
     * For detail of this object, see SafetyByteArray.
     * byte array in SafetyByteArray constraints:
     * Range: 0 ... byteLength (expected byte array size)
     * Unit: byte
     * Scaling: 1
     * 
     * throw IllegalStateException if port index is wrong, driver error occurs or expected data length is out of length.
     */
    @Override
    public SafetyByteArray read(int port, int byteLength) throws IllegalStateException
    {
        byte[] data = new byte[byteLength];
        byte[] returnData = new byte[ARRAY_INITIAL_SIZE];
        SafetyByteArray safetyData = new SafetyByteArray(returnData, CRCTool.generateCRC16(returnData));
        boolean isLengthLegal = (byteLength > NOT_ALLOWED_READ_BOTTOM_RANGE) && (byteLength < NOT_ALLOWED_READ_TOP_RANGE);
        
        try
        {
            if(isLengthLegal == true)
            {
                int returnLength = 0;
                
                returnLength = readUART(port, data);
                returnData = Arrays.copyOf(data, returnLength);
                safetyData = new SafetyByteArray(returnData, CRCTool.generateCRC16(returnData));
            }
            else
            {
                throw new IllegalStateException("expected data length is out of length.");
            }
        }
        catch (OperationFailException e)
        {
            e.printStackTrace();
            
            throw new IllegalStateException(e.getMessage());
        }
        finally
        {
            // Empty for static code analysis
        }
        
        return safetyData;
    }
    
    // ---------------------------------------------------------
    // Java native methods 
    // --------------------
    /**
     * Open UART port.
     * 
     * @param port [in] port index in UARTPort.
     * Range: COMMSUART, BGMUART in UARTPort
     * Unit: int
     * Scaling: 1
     * 
     * return None 
     * 
     * throw OperationException when UART opening fails.
     */
    private native final void openUART(int port) throws OperationFailException;
    
    /**
     * Close UART port.
     * 
     * @param port [in] port index in UARTPort.
     * Range: COMMSUART, BGMUART in UARTPort
     * Unit: int
     * Scaling: 1
     * 
     * return None 
     * 
     * throw OperationException when UART closing fails.
     */
    private native final void closeUART(int port) throws OperationFailException;
    
    /**
     * Write data into UART port.
     * 
     * @param port [in] port index in UARTPort.
     * Range: COMMSUART, BGMUART in UARTPort
     * Unit: int
     * Scaling: 1
     * @param data [in] safety byte array that is sending data.
     * byte array in SafetyByteArray:
     * Range: 0 ... unlimited
     * Unit: byte
     * Scaling: 1
     * 
     * return None 
     * 
     * throw OperationException when UART writing fails.
     */
    private native final void writeUART(int port, byte[] data) throws OperationFailException;
    
    /**
	 * Read data from UART port.
	 * 
	 * @param port    [in] port index in UARTPort.
	 * Range: COMMSUART, BGMUART in UARTPort
     * Unit: int
     * Scaling: 1
	 * @param data    [in] a byte array to store data. 
	 * Range: 1 ... 512 (max size of byte array)
     * Unit: byte
     * Scaling: 1
	 * 
	 * return integer that is data size.
	 * Range: 0 ... 512 (read byte size)
     * Unit: byte
     * Scaling: 1
     * 
     * throw OperationException when UART reading fails.
	 */
    private native final int readUART(int port, byte[] data) throws OperationFailException;
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
