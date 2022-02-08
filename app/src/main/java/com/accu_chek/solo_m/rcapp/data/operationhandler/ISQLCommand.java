/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.cgmapp.data.ISQLCommand
 * Brief:Interface for all command implement.
 * 
 * Create Date: 2013/12/30
 * $Revision: 15209 $
 * $Author: henrytso $
 * $Id: ISQLCommand.java 15209 2015-08-23 03:31:51Z henrytso $
 */

package com.accu_chek.solo_m.rcapp.data.operationhandler;

public interface ISQLCommand<T>
{
    /**
     * Execute a certain command.
     * 
     * @return T : The execution result in generic type.
     * 
     *         1. If the return type is Integer, it means the updated or deleted
     *         record count.
     *         Range: 0 ~ 5000
     *         Unit: Integer
     *         Scaling: 1
     * 
     *         2. if the return type is Uri, it means the path of the newly
     *         created row.
     *         Range: valid object
     *         Unit: Uri
     *         Scaling: 1
     * 
     *         3. If the return type is ArrayList, it means the query result.
     *         Range: null / valid object
     *         Unit: ArrayList
     *         Scaling: 1
     */
    public T execute();

}
