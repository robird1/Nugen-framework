/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.util.State
 * Brief: 
 *
 * Create Date: 2015¦~5¤ë29¤é
 * $Revision: 20555 $
 * $Author: DWYang $
 * $Id: State.java 20555 2015-10-01 13:50:22Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.util;

import android.os.Message;

/**
 * {@hide}
 *
 * The class for implementing states in a StateMachine
 */
public class State implements IState
{

    /**
     * Construct
     */
    protected State()
    {       
    }
    
    
    @Override
    public void enter()
    {      
    }

    @Override
    public void exit()
    {        
    }

    @Override
    public boolean processMessage(Message msg)
    {
        return false;
    }

    /**
     * Name of State for debugging purposes.
     *
     * This default implementation returns the class name, returning
     * the instance name would better in cases where a State class
     * is used for multiple states. But normally there is one class per
     * state and the class name is sufficient and easy to get. You may
     * want to provide a setName or some other mechanism for setting
     * another name if the class name is not appropriate.
     *
     * @see com.android.internal.util.IState#processMessage(android.os.Message)
     */
    @Override
    public String getName()
    {
        String name = getClass().getName();
        int lastDollar = name.lastIndexOf('$');
        return name.substring(lastDollar + 1);
    }

}
