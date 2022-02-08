/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.cgmapp.application.exception.DataTypeMismatchException
 * Brief: Thrown when there is data type mismatch problem happened
 * 
 * Create Date: 2015/3/16
 * Author: Jackson Huang
 */

package com.accu_chek.solo_m.rcapp.application.exception;

public class DataTypeMismatchException extends Exception 
{
    /**
     * generated serial version id
     * This ID is generaled by Android. DON't change to upper case
     */
    private static final long serialVersionUID = -3362881521595634490L;
    
    /**
     * default exception message
     */
    private static final String DEFAULT_MESSAGE = "Data type mismatch exception";
    
    /**
     * Constructs a new DataTypeMismatchException with the current stack trace and
     * the default message.
     */
    public DataTypeMismatchException()
    {
        super(DEFAULT_MESSAGE);
    }

    /**
     * Constructs a new DataTypeMismatchException with the current stack trace and
     * the specified detail message.
     * 
     * @param detailMessage [in] the detail message for this exception.
     */
    public DataTypeMismatchException(String detailMessage)
    {
        super(detailMessage);
    }
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
