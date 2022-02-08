/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.segmenthandler.IContinuaSegmentHandler
 * Brief: 
 *
 * Create Date: 2015/3/23
 * $Revision: 20933 $
 * $Author: kevenwu $
 * $Id: IContinuaSegmentHandler.java 20933 2015-10-05 08:34:57Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.segmenthandler;

import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;

/**
 * This class is used to implement a segment handler for processing the command
 * of each segment.
 */
public interface IContinuaSegmentHandler
{
    /**
     * Get required data and transfer to Continua Agent via ContinuaCommandController.
     *
     * @param commandSet : The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     *        
     * return void [out]: None        
     */
    void getData(ContinuaCommandSet commandSet);
    
    /**
     * Calculate the count of required data and transfer to Continua Agent via 
     * ContinuaCommandController.
     *
     * @param commandSet : The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     *        
     * return void [out]: None        
     */
    void getCount(ContinuaCommandSet commandSet);
}
