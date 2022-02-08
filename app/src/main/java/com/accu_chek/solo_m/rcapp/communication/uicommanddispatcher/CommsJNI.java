package com.accu_chek.solo_m.rcapp.communication.uicommanddispatcher;


/**
 * The CommsJNI provides interfaces to power on/off Comms subsystem and to
 * generate CRC which shares same algorithm with Comms subsystem.
 * 
 */

public final class CommsJNI
{

    static 
    {
        System.loadLibrary("custframeworksvr_jni");
    }
    
    /**
     * Constructor
     * 
     * @throws Exception Instance creation is not allowed.
     */
    protected CommsJNI() throws Exception
    {
        throw new Exception();
    }

    /**
     * The interface powers on Comms subsystem.
     * 
     * @return result The result of powering on Comms subsystem.
     *         Range: 0/-1
     *         Unit: int
     *         Scaling: 1
     */
    public static int powerOnComms()
    {
        int result = 0;

        result = _powerOn();

        return result;
    }

    /**
     * The interface powers off Comms subsystem.
     * 
     * @return result The result of powering off Comms subsystem.
     *         Range: 0/-1
     *         Unit: int
     *         Scaling: 1
     */

    public static int powerOffComms()
    {
        int result = 0;

        result = _powerOff();

        return result;
    }

    /**
     * The native jni interface powers on Comms subsystem.
     * 
     * @return result The result of powering on Comms subsystem.
     *         Range: 0/-1
     *         Unit: int
     *         Scaling: 1
     */
    private native static int _powerOn();

    /**
     * The native jni interface powers off Comms subsystem.
     * 
     * @return result The result of powering off Comms subsystem.
     *         Range: 0/-1
     *         Unit: int
     *         Scaling: 1
     */

    /**
     * Function Description
     *
     * @return int [out] 
     */
    private native static int _powerOff();

}
// [BT] Add header and footer for BLEUC02.java
