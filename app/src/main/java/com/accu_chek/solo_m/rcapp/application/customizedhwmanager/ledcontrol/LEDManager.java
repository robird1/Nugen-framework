/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name:
 * com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol.LEDManager
 * Brief:
 * The class provides an interface to modules in RC APP to control LED.
 * 
 * Create Date: 2015/1/19
 * $Revision: 20525 $
 * $Author: DWYang $
 * $Id: LEDManager.java 20525 2015-10-01 11:16:30Z DWYang $
 */
package com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol;

import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol.ILED;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
 * The class provides an interface to modules in RC APP to control LED.
 */
public class LEDManager 
{
    /**
     * LED frequency configure name
     */
    private static final int LED_FLASH_FREQUENCY = 5000;
    
    /**
     * LED service interface
     */
    private ILED mService = null;
    
    /**
     * LED type
     */
    public interface LEDTYPE
    {
        /**
         * The LED is lighted for reminding actions about insulin.
         */
        int INSULIN = 0x000F;
        
        /**
         * The LED is at the entrance which inserts a stripe. 
         */
        int STRIP = 0x0033;
        
        /**
         * The LED is for reminding phone's user to watch information.
         */
        int INFORMATION = 0x003C;
    }
    
    /**
     * Get LED interface that is used by RC APP framework service. 
     * The interface is provided RC APP framework service to generate background
     * LED service. 
     * 
     * @param None [in] 
     * 
     * return ILED that is the interface of LED. For detail of this object, see LED class.
     * Range: Valid ILED object
     * Unit: ILED
     * Scaling: 1
     */
    public static ILED getInterface()
    {
        return new LED();
    }
    
    /**
     * Store the interface of LED in local variable.
     * 
     * see mService [in] This global variable is referred for saving LED service.
     * 
     * @param service [in] the interface of LED. For detail of this object, see LED class.
     * Range: Valid ILED Object
     * Unit: ILED
     * Scaling: 1
     */
	public LEDManager(ILED service)
	{
	    mService = service;
	}

	/**
	 * Light the LED on based on given LED type. If any error occurs, EMWR warning is shown; log error message.
	 * 
	 * see mService [in] This global variable is referred for using LED service.
	 * 
	 * @param ledtype [in] LED type index in LEDTYPE
	 * Range: INSULIN, STRIP or INFORMATION in LEDTYPE
     * Unit: int
     * Scaling: 1 
	 * 
	 * return None
	 * @throws OperationFailException when fail to open LED.
	 */
	public void openLED(int ledtype) throws OperationFailException
	{
	    try
        {
            mService.open(ledtype);
        }
        catch (RemoteException e)
        {
            Debug.printI("LEDManager", "RemoteException");
            
            e.printStackTrace();
            
            throw new OperationFailException("Fail to open LED");    
            
         }
	    catch (IllegalStateException e)
	    {
	        e.printStackTrace();
            
            throw new OperationFailException("Fail to open LED");     
	    }
	    catch (NullPointerException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException("LED manager is null");     
        }
	    finally
	    {
	        // Empty for static code analysis
	    }
	}

	/**
	 * Turn the LED off based on given LED type. If any error occurs, EMWR warning is shown; log error message.
	 * 
	 * see mService [in] This global variable is referred for using LED service.
	 * 
	 * @param ledtype [in] LED type index in LEDTYPE 
	 * Range: INSULIN, STRIP or INFORMATION in LEDTYPE
     * Unit: int
     * Scaling: 1
	 * 
	 * return None
	 * @throws OperationFailException when fail to close LED.
	 */
	public void closeLED(int ledtype) throws OperationFailException
	{
	    try
        {
            mService.close(ledtype);
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException("Fail to close LED");    
        }
	    catch (IllegalStateException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException("Fail to close LED");    
        }
	    catch (NullPointerException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException("LED manager is null");     
        }
	    finally
	    {
	        // Empty for static code analysis
	    }
	}

	/**
	 * Flash LED based on given LED type. If any error occurs, EMWR warning is shown; log error message.
	 * LED Flash frequency is defined in SOLO M EMWR Specification, e.g. 200 mHz.
	 * 
	 * see mService [in] This global variable is referred for using LED service.
	 * 
	 * @param ledtype [in] LED type index in LEDTYPE, but only information LED can flash currently.
	 * Range: INFORMATION in LEDTYPE
     * Unit: int
     * Scaling: 1
	 * 
	 * return None
	 * @throws OperationFailException when fail to flash LED.
	 */
	public void flashLED(int ledtype) throws OperationFailException
	{
	    try
        {
	        SafetyNumber<Integer> frequency = new SafetyNumber<Integer>(LED_FLASH_FREQUENCY, -LED_FLASH_FREQUENCY);
	        
            mService.flash(frequency, ledtype);
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException("Fail to flash LED");    
        }
	    catch (IllegalStateException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException("Fail to flash LED");    
        }
	    catch (NullPointerException e)
        {
            e.printStackTrace();
            
            throw new OperationFailException("LED manager is null");     
        }
	    finally
	    {
	        // Empty for static code analysis
	    }
	}

}