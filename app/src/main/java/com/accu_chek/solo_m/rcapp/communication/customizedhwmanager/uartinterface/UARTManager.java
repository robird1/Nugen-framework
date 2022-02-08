/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.customizedhwmanager.uartinterface.UARTManager
 * Brief: 
 * The class provides an interface to modules in RC APP to read/write data via UART.
 *
 * Create Date: 2015/1/19
 * $Revision: 20525 $
 * $Author: DWYang $
 * $Id: UARTManager.java 20525 2015-10-01 11:16:30Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.communication.customizedhwmanager.uartinterface;

import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;

/**
 * The class provides an interface to modules in RC APP to read/write data via UART.
 */
public final class UARTManager
{
    /**
     * intent action of closing UART. When an broadcast intent with this action is sent, both of COMMS UART port and BGM UART port are closed. 
     */
    public static final String CLOSE_UART = "com.accu_chek.solo_m.rcapp.uartservice.close";
    /**
     * UICP interface
     */
    private IUART mService = null;

    /**
     * allowed UART ports
     */
    public interface UARTPort
    {
        /**
         * Communication with BLE board via UART
         */
        public int COMMSUART = 0x000F;
        
        /**
         * Communication with BGM board via UART
         */
        public int BGMUART = 0x0033;
    }
    
    /**
     * Get UART interface that is used by RC APP framework service. 
     * The interface is provided RC APP framework service to generate background UART service. 
     *
     * @param None [in] 
     * 
     * return IUART that is the interface of UART. For detail of this object, see UART class.
     * Range: Valid IUART object
     * Unit: IUART
     * Scaling: 1
     */
    public static IUART getInterface()
    {
        return new UART();
    }

    /**
     * Store the interface of UART in local variable.
     * 
     * see mService [in] This global variable is referred for saving UART service.
     * 
     * @param service [in] the interface of UART. For detail of this object, see UART class.
     * Range: Valid IUART Object
     * Unit: IUART
     * Scaling: 1
     */
    public UARTManager(IUART service)
    {
        mService = service;
        
    }
    
    /**
     * Open UART port.
     * 
     * see mService [in] This global variable is referred for getting UART service.
     *
     * @param port [in] port index in UARTPort.
     * Range: COMMSUART, BGMUART in UARTPort
     * Unit: int
     * Scaling: 1
     * 
     * throw OperationFailException when opening port has any exceptions.
     * 
     * return None 
     */
    public void open(int port) throws OperationFailException
    {
        try
        {
            mService.open(port);
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException(e.getMessage());
        }
        catch (IllegalStateException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException(e);
        }
        finally
        {
            // Empty for static code analysis
        }
    }
    
    /**
     * Close UART port.
     * 
     * see mService [in] This global variable is referred for getting UART service.
     *
     * @param port [in] port index in UARTPort.
     * Range: COMMSUART, BGMUART in UARTPort
     * Unit: int
     * Scaling: 1
     * 
     * throw OperationFailException when closing port has any exceptions.
     * 
     * return None 
     */
    public void close(int port) throws OperationFailException
    {
        try
        {
            mService.close(port);
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException(e.getMessage());
        }
        catch (IllegalStateException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException(e);
        }
        finally
        {
            // Empty for static code analysis
        }
    }
    
    /**
     * Call UART interface to send data into a port.
     * 
     * see mService [in] This global variable is referred for getting UART service.
     *
     * @param port [in] port index in UARTPort.
     * Range: COMMSUART, BGMUART in UARTPort
     * Unit: int
     * Scaling: 1
     * @param data [in] safety byte array of sending data
     * byte array in SafetyByteArray:
     * Range: 0 ... max integer
     * Unit: byte
     * Scaling: 1
     * 
     * throw OperationFailException when sending data has any exceptions.
     * 
     * return None 
     */
    public void send(int port, SafetyByteArray data) throws OperationFailException
    {
        try
        {
            mService.write(port, data);
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException(e.getMessage());
        }
        catch (IllegalStateException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException(e);
        }
        finally
        {
            // Empty for static code analysis
        }
    }
    
    /**
     * Call UART interface to receive data from a port.
     * 
     * see mService [in] This global variable is referred for getting UART service.
     * see UART.ALLOWED_READ_MIN_RANGE [in] This global variable is referred for not allowed read minimum range
     * see UART.ALLOWED_READ_MAX_RANGE [in] This global variable is referred for not allowed read maximum range
     *
     * @param port [in] port index in UARTPort.
     * Range: COMMSUART, BGMUART in UARTPort
     * Unit: int
     * Scaling: 1
     * @param expectedByteLength [in] expected byte array size.
     * Range: 1 ... 512
     * Unit: byte
     * Scaling: 1
     * 
     * throw OperationFailException when receiving data has any exceptions in UART driver.
     * throw ArgumentErrorException when expected byte length is out of range.
     * 
     * return SafetyByteArray that stores byte array and is received from the port. 
     * If there is no any data from UART port, return a byte array with zero length in safety byte array. For detail of this object, see SafetyByteArray.
     * byte array in SafetyByteArray constraints:
     * Range: 0 ... expectedByteLength (expected byte array size)
     * Unit: byte
     * Scaling: 1
     *  
     */
    public SafetyByteArray receive(int port, int expectedByteLength) throws OperationFailException, ArgumentErrorException
    {
        SafetyByteArray readData = null;
        
        if ((expectedByteLength > UART.NOT_ALLOWED_READ_BOTTOM_RANGE) && (expectedByteLength < UART.NOT_ALLOWED_READ_TOP_RANGE))
        {
            try
            {
                readData = mService.read(port, expectedByteLength);
            }
            catch (RemoteException e)
            {
                e.printStackTrace();
                
                throw new OperationFailException(e.getMessage());
            }
            catch (IllegalStateException e)
            {
                e.printStackTrace();
                
                throw new OperationFailException(e);
            }
            finally
            {
                // Empty for static code analysis
            }
        }
        else
        {
            throw new ArgumentErrorException("Expected byte length should be between 1 and 512.");
        }
        
        return readData;
    }
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
