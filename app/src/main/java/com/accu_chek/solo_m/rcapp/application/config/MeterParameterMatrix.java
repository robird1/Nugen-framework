/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.config.MeterParameterMatrix
 * Brief: Provide System Constants for Application Usage
 *
 * Create Date: 03/03/2015
 * $Revision: 23369 $
 * $Author: WilliyChiang $
 * $Id: MeterParameterMatrix.java 23369 2015-11-05 07:40:14Z WilliyChiang $
 */
package com.accu_chek.solo_m.rcapp.application.config;

public class MeterParameterMatrix extends MeterParameterMatrixBody 
{
    // Class Instance
    private static MeterParameterMatrix mInstance = null;
    
    private MeterParameterMatrix()
    {
        // load Meter Parameter from File
        super.init();
    }
    
    /**
     * Return class instance. If instance is not existed, create a new one
     *  
     * @param None 
     * @return MeterParameterMatrix [out] Instance of MeterParameterMatrix
     *              Range : Valid MeterParameterMatrix object
     *              Unit: MeterParameterMatrix
     *              Scaling: 1
     * 
     */    

    public synchronized static MeterParameterMatrix getMeterParameterMatrixInstance()
    {
        // If Instance is null, create new one, else return existed one
        if(mInstance == null)
        {
            mInstance = new MeterParameterMatrix();        
        }
        
        return mInstance;
    }
    
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */

// Update comments for wording refine
