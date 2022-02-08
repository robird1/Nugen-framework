/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name:
 * com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception
 * .RDYDessertException
 * Brief:
 *
 * Create Date: 2015¦~4¤ë9¤é
 * $Revision: 24486 $
 * $Author: VictorChen $
 * $Id: RDYDessertException.java 24486 2015-11-20 05:48:56Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception;

public class RDYDessertException extends Exception
{

    /**
     * generated serial version id
     * This ID is generaled by Android. DON't change to upper case
     */
    private static final long serialVersionUID = 6992421980304147485L;
    /**
     * default exception message
     */
    private static final String DEFAULT_MESSAGE = "BGM RDY pin deassert";

    /**
     * Construct a new RDYDessertException with the current stack trace and
     * the specified detail message.
     * 
     * return void [out] None.
     * 
     * @param errorDescription [in] exception message.
     *            Range: valid object.
     *            Unit: String
     *            Scaling: 1
     */
//    public RDYDessertException(String errorDescription)
//    {
//        super(errorDescription);
//    }

    /**
     * Construct a new RDYDessertException with the current stack trace and
     * the default message.
     * return void [out] None.
     */
    public RDYDessertException()
    {
        super(DEFAULT_MESSAGE);
    }

}
// (R20520 2015-10-01 07:04:11 DWYang)
// ----------------------------------------------------------------------------
// Refine code comment.
