/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LogData
 * Brief: The class is used to store the formatted log data.
 *
 * Create Date: 08/26/2015
 * $Revision: 21903 $
 * $Author: WilliyChiang $
 * $Id: LogData.java 21903 2015-10-19 07:31:17Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

public class LogData
{
    
    /**
     * The content of LogData
     */    
    private String mContent = null;

    /**
     * Get the content of LogData object.
     * 
     * @param None
     * 
     * @return The result
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1
     */
    public final String getContent()
    {
        // Return content in LogData object
        return mContent;
    }

    /**
     * Set the content of LogData object.
     * 
     * @param content The log information.
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1
     * 
     * @return None
     */
    public final void setContent(final String content)
    {
        // Set content of LogData object
        mContent = content;
    }
    
}
