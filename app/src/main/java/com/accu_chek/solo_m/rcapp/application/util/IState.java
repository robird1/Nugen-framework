/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.util.IState
 * Brief: 
 *
 * Create Date: 2015¦~5¤ë29¤é
 * $Revision: 20555 $
 * $Author: DWYang $
 * $Id: IState.java 20555 2015-10-01 13:50:22Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.util;

import android.os.Message;


/**
 *
 * The interface for implementing states in a {@link StateMachine}
 */
public interface IState
{
    /**
     * Returned by processMessage to indicate the the message was processed.
     */
    static final boolean HANDLED = true;

    /**
     * Returned by processMessage to indicate the the message was NOT processed.
     */
    static final boolean NOT_HANDLED = false;

    /**
     * Called when a state is entered.
     */
    void enter();

    /**
     * Called when a state is exited.
     */
    void exit();

    /**
     * Called when a message is to be processed by the
     * state machine.
     *
     * This routine is never reentered thus no synchronization
     * is needed as only one processMessage method will ever be
     * executing within a state machine at any given time. This
     * does mean that processing by this routine must be completed
     * as expeditiously as possible as no subsequent messages will
     * be processed until this routine returns.
     *
     * @param msg to process
     * @return HANDLED if processing has completed and NOT_HANDLED
     *         if the message wasn't processed.
     */
    boolean processMessage(Message msg);

    /**
     * Name of State for debugging purposes.
     *
     * @return name of state.
     */
    String getName();
}
