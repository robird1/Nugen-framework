/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.customizedhwmanager.exception.OprationFailException
 * Brief: Thrown when there is customized HW manager problem or UICP problem.
 *
 * Create Date: 2015/1/20
 * $Revision: 20527 $
 * $Author: DWYang $
 * $Id: OperationFailException.java 20527 2015-10-01 11:17:29Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.exception;

/**
 * Thrown when there is customized HW manager problem or UICP problem.
 */
public class OperationFailException extends Exception
{
    /**
     * generated serial version id
     * This ID is generaled by Android. DON't change to upper case 
     */
    private static final long serialVersionUID = -1301059246486088560L;

    /**
     * default exception message
     */
    private static final String DEFAULT_MESSAGE = "Fail to execute the operation";
    
    /**
     * Construct a new OperationFailException with the current stack trace and
     * the default message.
     */
    public OperationFailException()
    {
        super(DEFAULT_MESSAGE);
    }
    
    /**
     * Construct a new OperationFailException with the current stack trace and
     * the specified detail message.
     * 
     * @param detailMessage [in] the detail message for this exception. String object provided by Android SDK.
     */
    public OperationFailException(String detailMessage)
    {
        super(detailMessage);
    }
    
    /**
     * Construct a new OperationFailException with the current stack trace and
     * the specified cause.
     * 
     * @param exception [in] the cause of this exception. Exception object provided by Android SDK.
     */
    public OperationFailException(Exception exception)
    {
        super(exception);
    }
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
