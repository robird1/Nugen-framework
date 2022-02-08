package com.accu_chek.solo_m.rcapp.communication.customizedhwmanager.uartinterface;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;

/**
 * {@hide}
 */
interface IUART
{
    //export function
    /**
     * Open UART port.
     * 
     * @param port [in] port index in UARTPort.
     * Range: COMMSUART, BGMUART in UARTPort
     * Unit: int
     * Scaling: 1
     * @return None 
     * @throw OperationFailException if port index is wrong or driver error occurs
     */
    void open(in int port);
    /**
     * Close UART port.
     * 
     * @param port [in] port index in UARTPort.
     * Range: COMMSUART, BGMUART in UARTPort
     * Unit: int
     * Scaling: 1
     * @return None 
     * @throw OperationFailException if port index is wrong or driver error occurs
     */
    void close(in int port);
    /**
     * Write data into UART port.
     * 
     * @param port [in] port index in UARTPort.
     * Range: COMMSUART, BGMUART in UARTPort
     * Unit: int
     * Scaling: 1
     * @param data [in] safety byte array that is sending data. For detail of this object, see SafetyByteArray.
     * byte array in SafetyByteArray:
     * Range: 0 ... max integer
     * Unit: byte
     * Scaling: 1
     * @return None 
     * @throw OperationFailException if port index is wrong or driver error occurs
     */
    void write(in int port, in SafetyByteArray data);
    /**
     * Read data from UART port.
     * 
     * @param port [in] port index in UARTPort.
     * Range: COMMSUART, BGMUART in UARTPort
     * Unit: int
     * Scaling: 1
     * @param byteLength [in] expected byte array size.
     * Range: 1 ... 512
     * Unit: byte
     * Scaling: 1
     * @return SafetyByteArray Return byte array that is received from the port. 
     * If there is no any data from UART port, return a byte array with zero length in safety byte array. For detail of this object, see SafetyByteArray.
     * byte array in SafetyByteArray constraints:
     * Range: 0 ... byteLength (expected byte array size)
     * Unit: byte
     * Scaling: 1
     * @throw OperationFailException if port index is wrong or driver error occurs
     */
    SafetyByteArray read(in int port, in int byteLength);
}
