/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.uartuicp.UICPManager
 * Brief: 
 * The class provides an interface to modules in RC APP to control UICP.
 *
 * Create Date: 2015/2/2
 * $Revision: 20554 $
 * $Author: DWYang $
 * $Id: UICPControl.java 20554 2015-10-01 13:47:25Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.communication.uartuicp;

import com.accu_chek.solo_m.rcapp.communication.uartuicp.IUICP;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;

import android.os.RemoteException;


/**
 * The class provides an interface to modules in RC APP to control UICP.
 */
public final class UICPControl
{
    /**
     * UICP interface
     */
    private IUICP mService = null;
    
    /**
     * Get UICP interface.
     *
     * @param None [in] 
     * 
     * return IUICP that is the interface of UICP. For detail of this object, see UICP class.
     * Range: Valid IUICP Object
     * Unit: IUICP
     * Scaling: 1
     */
    public static IUICP getInterface()
    {
        return new UICP();
    }
    
    /**
     * Store the interface of UICP in local variable.
     * @param service [in] the interface of UICP. For detail of this object, see UICP class.
     * Range: Valid IUICP Object
     * Unit: IUICP
     * Scaling: 1
     */
    public UICPControl(IUICP service)
    {
        mService = service;
    }
    
    /**
     * Call UICP interface to reset UICP.
     * 
     * @param None [in]
     * 
     * return None 
     */
    void reset() throws OperationFailException
    {
        try
        {
            mService.reset();
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException(e.getMessage());
        }
        finally
        {
            // the section does nothing
        }
    }

    /**
     * Call UICP interface to open Tx.
     * 
     * @param None [in]
     * 
     * return None 
     * 
     * throw OperationFailException if UICP interface has any exceptions.
     */
    void openTx() throws OperationFailException
    {
        try
        {
            mService.openTx();
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
            // the section does nothing
        }
    }
    
    /**
     * Call UICP interface to close Tx.
     * 
     * @param None [in]
     * 
     * return None 
     * 
     * throw OperationFailException if UICP interface has any exceptions.
     */
    void closeTx() throws OperationFailException
    {
        try
        {
            mService.closeTx();
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
            // the section does nothing
        }
    }
    
    /**
     * Call UICP interface to open Rx.
     * 
     * @param None [in]
     * 
     * return None 
     * 
     * throw OperationFailException if UICP interface has any exceptions.
     */
    void openRx() throws OperationFailException
    {
        try
        {
            mService.openRx();
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
            // the section does nothing
        }
    }
    
    /**
     * Call UICP interface to close Rx.
     * 
     * @param None [in]
     * 
     * return None 
     * 
     * throw OperationFailException if UICP interface has any exceptions.
     */
    void closeRx() throws OperationFailException
    {
        try
        {
            mService.closeRx();
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
            // the section does nothing
        }
    }
    
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
    int getSendingStatus() throws OperationFailException
    {
        int shouldStopReceive = -1;
        
        try
        {
            shouldStopReceive = mService.getSendingStatus();
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException(e);
        }
        catch (IllegalStateException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException(e);
        }
        finally
        {
            // the section does nothing
        }
        
        return shouldStopReceive;
    }
    /**
     * Set UICP as function mode.
     */
    void seFunctionMode() throws OperationFailException
    {
        try
        {
            mService.seFunctionMode();
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
            // the section does nothing
        }
    }
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
