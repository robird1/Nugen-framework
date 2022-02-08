/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name:
 * uartuicp.UICP
 * Brief:
 * The class provides inner interface to UICPManager to call the function in JNI layer.
 * 
 * Create Date: 2015/2/2
 * $Revision: 20554 $
 * $Author: DWYang $
 * $Id: UICP.java 20554 2015-10-01 13:47:25Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.communication.uartuicp;

import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.communication.uartuicp.IUICP;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;

/**
 * The class provides inner interface to UICPManager to call the function in JNI layer.
 */
class UICP extends IUICP.Stub
{
    /**
     * hardware uevent state
     */
    private final String UEVT_STATE = "com.accu_chek.hardware.uevtservice.action.uevent";
    
    /**
     * sending status of remote system
     */
    public interface REMOTE_SEND_STATUS
    {
        /**
         * stop sending
         */
        int STOP = 0x000F;
        /**
         * sending
         */
        int SENDING = 0x0033;
    }

    /**
     * Reset UICP
     * 
     * @param None [in]
     * 
     * return None 
     */
    public void reset()
    {
        resetUICP();
    }

    /**
     * Open UICP Tx.
     * 
     * @param None [in]
     * 
     * return None 
     * 
     * throw IllegalStateException if UICP interface has any exceptions in UICP driver layer.
     */
    public void openTx() throws IllegalStateException
    {
        try
        {
            openUICPTx();
        }
        catch(OperationFailException e)
        {
            e.printStackTrace();
            
            throw new IllegalStateException(e);
        }
        finally
        {
            // the section does nothing
        }
    }

    /**
     * Close UICP Tx.
     * 
     * @param None [in]
     * 
     * return None 
     * 
     * throw IllegalStateException if UICP interface has any exceptions in UICP driver layer.
     */
    public void closeTx() throws IllegalStateException
    {
        try
        {
            closeUICPTx();
        }
        catch(OperationFailException e)
        {
            e.printStackTrace();
            
            throw new IllegalStateException(e);
        }
        finally
        {
            // the section does nothing
        }
    }

    /**
     * Open UICP Rx.
     * 
     * @param None [in]
     * 
     * return None 
     * 
     * throw IllegalStateException if UICP interface has any exceptions in UICP driver layer.
     */
    public void openRx() throws IllegalStateException
    {
        try
        {
            openUICPRx();
        }
        catch(OperationFailException e)
        {
            e.printStackTrace();
            
            throw new IllegalStateException(e);
        }
        finally
        {
            // the section does nothing
        }
    }

    /**
     * Close UICP Rx.
     * 
     * @param None [in]
     * 
     * return None 
     * 
     * throw IllegalStateException if UICP interface has any exceptions in UICP driver layer.
     */
    public void closeRx() throws IllegalStateException
    {
        try
        {
            closeUICPRx();
        }
        catch(OperationFailException e)
        {
            e.printStackTrace();
            
            throw new IllegalStateException(e);
        }
        finally
        {
            // the section does nothing
        }
    }
    
    /**
     * Get the sending status of COMMS.
     * 
     * @param None [in]
     * 
     * return integer index in REMOTE_SEND_STATUS. Return STOP if COMMS sending stop. Otherwise, return SENDING.
     * Range: STOP or SENDING in REMOTE_SEND_STATUS
     * Unit: int
     * Scaling: 1
     * 
     * throw IllegalStateException if UICP interface has any exceptions in UICP driver layer.
     */
    @Override
    public int getSendingStatus() throws RemoteException
    {
        int stopFlag = REMOTE_SEND_STATUS.STOP;
        
        try
        {
            stopFlag = getCOMMSSendingStatus();
        }
        catch(OperationFailException e)
        {
            e.printStackTrace();
            
            throw new IllegalStateException(e);
        }
        finally
        {
            // the section does nothing
        }
        
        return stopFlag;
    }

    /**
     * Set UICP as function mode.
     */
    @Override
    public void seFunctionMode()
    {
        setUICPFunctionMode();
    }

    // ---------------------------------------------------------
    // Java native methods
    // --------------------
    
    /**
     * Reset UICP
     * 
     * @param None [in]
     * 
     * return None 
     */
    protected native void resetUICP();

    /**
     * Open UICP Tx.
     * 
     * @param None [in]
     * 
     * return None 
     * 
     * throw OperationFailException if UICP interface has any exceptions in UICP driver layer.
     */
    protected native void openUICPTx() throws OperationFailException;

    /**
     * Close UICP Tx.
     * 
     * @param None [in]
     * 
     * return None 
     * 
     * throw OperationFailException if UICP interface has any exceptions in UICP driver layer.
     */
    protected native void closeUICPTx() throws OperationFailException;

    /**
     * Open UICP Rx.
     * 
     * @param None [in]
     * 
     * return None 
     * 
     * throw OperationFailException if UICP interface has any exceptions in UICP driver layer.
     */
    protected native void openUICPRx() throws OperationFailException;

    /**
     * Close UICP Rx.
     * 
     * @param None [in]
     * 
     * return None 
     * 
     * throw OperationFailException if UICP interface has any exceptions in UICP driver layer.
     */
    protected native void closeUICPRx() throws OperationFailException;
    
    /**
     * Get the sending status of COMMS.
     * 
     * @param None [in]
     * 
     * return integer index in REMOTE_SEND_STATUS. Return STOP if COMMS sending stop. Otherwise, return SENDING.
     * Range: STOP or SENDING in REMOTE_SEND_STATUS
     * Unit: int
     * Scaling: 1
     * 
     * throw OperationFailException if UICP interface has any exceptions in UICP driver layer.
     */
    protected native int getCOMMSSendingStatus() throws OperationFailException;

    /**
     * Set UICP as function mode.
     */
    protected native void setUICPFunctionMode();
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
