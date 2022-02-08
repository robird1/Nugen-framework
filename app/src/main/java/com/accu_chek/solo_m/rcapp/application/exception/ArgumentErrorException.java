/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.exception.ArgumentErrorException
 * Brief: 
 *
 * Create Date: 2015/4/15
 * $Revision: 20527 $
 * $Author: DWYang $
 * $Id: ArgumentErrorException.java 20527 2015-10-01 11:17:29Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.exception;

public class ArgumentErrorException extends Exception
{

    /**
     * generated serial version id
     * This ID is generaled by Android. DON't change to upper case
     */
    private static final long serialVersionUID = 5960330351956955289L;
    
    /**
     * default exception message
     */
    private static final String DEFAULT_MESSAGE = "Error input argument.";
    
    /**
     * Construct a new ArgumentErrorException with the current stack trace and
     * the default message.
     */
    public ArgumentErrorException()
    {
        super(DEFAULT_MESSAGE);
    }

    /**
     * Construct a new ArgumentErrorException with the current stack trace and
     * the specified detail message.
     * 
     * @param detailMessage [in] the detail message for this exception.
     */
    public ArgumentErrorException(String detailMessage)
    {
        super(detailMessage);
    }

}
