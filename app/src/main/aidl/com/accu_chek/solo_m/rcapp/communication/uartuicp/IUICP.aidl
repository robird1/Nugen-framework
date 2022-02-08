package com.accu_chek.solo_m.rcapp.communication.uartuicp;


/**
 * {@hide}
 */
interface IUICP
{
    //export function
    //export function
    /**
     * Reset UICP
     * 
     * @param None [in]
     * 
     * return None 
     */
    void reset();
    /**
     * Open UICP Tx.
     * 
     * @param None [in]
     * 
     * return None 
     *
     * throw OperationFailException if UICP interface has any exceptions in UICP driver layer.
     */
    void openTx();
    /**
     * Close UICP Tx.
     * 
     * @param None [in]
     *
     * return None 
     * 
     * throw OperationFailException if UICP interface has any exceptions in UICP driver layer.
     */
    void closeTx();
    /**
     * Open UICP Rx.
     * 
     * @param None [in]
     *
     * return None 
     *
     * throw OperationFailException if UICP interface has any exceptions in UICP driver layer.
     */
    void openRx();
    /**
     * Close UICP Rx.
     * 
     * @param None [in]
     * 
     * return None 
     * 
     * throw OperationFailException if UICP interface has any exceptions in UICP driver layer.
     */
    void closeRx();
    /**
     * Get COMMS sending status.
     * 
     * @param None [in]
     * 
     * return integer index in REMOTE_SEND_STATUS. Return STOP if COMMS sending stop. Otherwise, return SENDING.
     * Range: STOP or SENDING in REMOTE_SEND_STATUS
     * Unit: int
     * Scaling: 1
     * 
     * throw OperationFailException if UICP interface has any exceptions.
     */
    int getSendingStatus();
    /**
     * Set UICP as function mode.
     */
    void seFunctionMode();
}
