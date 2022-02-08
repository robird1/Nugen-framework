/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name:
 * com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol.LED
 * Brief:
 * The class provides inner interface to LEDManager to call the function in JNI layer.
 * 
 * Create Date: 2015/1/19
 * $Revision: 20525 $
 * $Author: DWYang $
 * $Id: LED.java 20525 2015-10-01 11:16:30Z DWYang $
 */
package com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol;

import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol.ILED;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
 * The class provides inner interface to LEDManager to call the function in JNI layer.
 */
class LED extends ILED.Stub{

	/**
	 * Turn the LED on based on given LED type.
	 * 
	 * @param type [in] LED type index in LEDTYPE
	 * Range: INSULIN, STRIP or INFORMATION in LEDTYPE
     * Unit: int
     * Scaling: 1
	 * 
	 * return None 
	 * 
	 * throw IllegalStateException caused by OperationFailExcpetion if LED type is wrong
	 */
	public void open(int type) throws IllegalStateException
	{
	    try
	    {
	        openLED(type);
	    }
	    catch (OperationFailException e)
	    {
	        Debug.printI("LED", "OperationFailException");
	        e.printStackTrace();
	        
	        throw new IllegalStateException(e.getMessage());
	    }
	    finally
	    {
	        // Empty for static code analysis
	    }
	}

	/**
	 * Close LED based on given LED type based on given LED type.
	 * 
	 * @param type [in] LED type index in LEDTYPE
	 * Range: INSULIN, STRIP or INFORMATION in LEDTYPE
     * Unit: int
     * Scaling: 1
	 * 
	 * return None 
	 * 
	 * throw IllegalStateException if LED type is wrong
	 */
	public void close(int type) throws IllegalStateException
	{
	    try
	    {
	        closeLED(type);
	    }
	    catch (OperationFailException e)
        {
            Debug.printI("LED", "OperationFailException");
            e.printStackTrace();
            
            throw new IllegalStateException(e.getMessage());
        }
        finally
        {
            // Empty for static code analysis
        }
	}

	/**
	 * Flash LED based on given LED type.
	 * 
	 * @param frequency [in] LED frequency.
     * Range: Setting in SOLO M EMWR Specification
     * Unit: int
     * Scaling: 1
	 * @param type [in] LED type index in LEDTYPE, but only information LED can flash currently.
	 * Range: INFORMATION in LEDTYPE
     * Unit: int
     * Scaling: 1
	 * 
	 * return None 
	 * 
	 * throw IllegalStateException if LED type is wrong
	 */
	@SuppressWarnings("rawtypes") 
	public void flash(SafetyNumber frequency, int type) throws IllegalStateException
	{
	    try
	    {
	        flashLED(frequency.get().intValue(), type);
    	}
        catch (OperationFailException e)
        {
            Debug.printI("LED", "OperationFailException");
            e.printStackTrace();
            
            throw new IllegalStateException(e.getMessage());
        }
        finally
        {
            // Empty for static code analysis
        }
	}
	
    // ---------------------------------------------------------
    // Java native methods 
    // --------------------
	/**
     * Native function. Turn the LED on based on given LED type.
     * 
     * @param ledType [in] LED type index in LEDTYPE
     * Range: INSULIN, STRIP or INFORMATION in LEDTYPE
     * Unit: int
     * Scaling: 1
     * 
     * return None 
     * 
     * throw OperationFailException if LED type is wrong
     */
    private native final void openLED(int ledType) throws OperationFailException;
    
    /**
     * Native function. Close LED based on given LED type.
     * 
     * @param ledType [in] LED type index in LEDTYPE
     * Range: INSULIN, STRIP or INFORMATION in LEDTYPE
     * Unit: int
     * Scaling: 1
     * 
     * return None 
     * 
     * throw OperationFailException if LED type is wrong
     */
    private native final void closeLED(int ledType) throws OperationFailException;
    
    /**
     * Native function. Flash LED based on given LED type.
     * 
     * @param frequency [in] LED frequency.
     * Range: Setting in SOLO M EMWR Specification
     * Unit: int
     * Scaling: 1
     * @param ledType [in] LED type index in LEDTYPE, but only information LED can flash currently.
     * Range: INFORMATION in LEDTYPE
     * Unit: int
     * Scaling: 1
     * 
     * return None 
     * 
     * throw OperationFailException if LED type is wrong
     */
    private native final void flashLED(int frequency, int ledType) throws OperationFailException;

}