/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: CustUEventObserver
 * Brief: 
 *
 * Create Date: 9/17/2015
 * $Revision: 20525 $
 * $Author: DWYang $
 * $Id: CustUEventObserver.java 20525 2015-10-01 11:16:30Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application;

import android.util.Log;

import java.util.ArrayList;
import java.util.HashMap;

import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
 * UEventObserver is an abstract class that receives UEvent's from the kernel.<p>
 *
 * Subclass UEventObserver, implementing onUEvent(UEvent event), then call
 * startObserving() with a match string. The UEvent thread will then call your
 * onUEvent() method when a UEvent occurs that contains your match string.<p>
 *
 * Call stopObserving() to stop receiving UEvent's.<p>
 *
 * There is only one UEvent thread per process, even if that process has
 * multiple UEventObserver subclass instances. The UEvent thread starts when
 * the startObserving() is called for the first time in that process. Once
 * started the UEvent thread will not stop (although it can stop notifying
 * UEventObserver's via stopObserving()).<p>
 *
 * @hide
*/
public abstract class CustUEventObserver {
    private static final String TAG = "CustUEventObserver";
    private static final boolean DEBUG = true;

    private static NugenUEventThread sThread;

    private static native void nativeSetup();
    private static native String nativeWaitForNextEvent();

    public CustUEventObserver() {
    }

    @Override
    protected void finalize() throws Throwable {
        try {
            stopObserving();
        } finally {
            super.finalize();
        }
    }

    private static NugenUEventThread getThread() {
        synchronized (CustUEventObserver.class) {
            if (sThread == null) {
                sThread = new NugenUEventThread();
                sThread.start();
            }
            return sThread;
        }
    }

    private static NugenUEventThread peekThread() {
        synchronized (CustUEventObserver.class) {
            return sThread;
        }
    }

    /**
     * Begin observation of UEvent's.<p>
     * This method will cause the UEvent thread to start if this is the first
     * invocation of startObserving in this process.<p>
     * Once called, the UEvent thread will call onUEvent() when an incoming
     * UEvent matches the specified string.<p>
     * This method can be called multiple times to register multiple matches.
     * Only one call to stopObserving is required even with multiple registered
     * matches.
     *
     * @param match A substring of the UEvent to match.  Try to be as specific
     * as possible to avoid incurring unintended additional cost from processing
     * irrelevant messages.  Netlink messages can be moderately high bandwidth and
     * are expensive to parse.  For example, some devices may send one netlink message
     * for each vsync period.
     */
    public final void startObserving(String match) {
        if (match == null || match.isEmpty()) {
            throw new IllegalArgumentException("match substring must be non-empty");
        }
        Debug.printI(TAG, "[startObserving] start NugenUEvent thread");
        final NugenUEventThread t = getThread();
        t.addObserver(match, this);
    }

    /**
     * End observation of UEvent's.<p>
     * This process's UEvent thread will never call onUEvent() on this
     * UEventObserver after this call. Repeated calls have no effect.
     */
    public final void stopObserving() {
        final NugenUEventThread t = getThread();
        if (t != null) {
            t.removeObserver(this);
        }
    }

    /**
     * Subclasses of UEventObserver should override this method to handle
     * UEvents.
     */
    public abstract void onUEvent(UEvent event);

    /**
     * Representation of a UEvent.
     */
    public static final class UEvent {
        // collection of key=value pairs parsed from the uevent message
        private final HashMap<String,String> mMap = new HashMap<String,String>();

        public UEvent(String message) {
            int offset = 0;
            int length = message.length();

            while (offset < length) {
                int equals = message.indexOf('=', offset);
                int at = message.indexOf('\0', offset);
                if (at < 0) break;

                if (equals > offset && equals < at) {
                    // key is before the equals sign, and value is after
                    mMap.put(message.substring(offset, equals),
                            message.substring(equals + 1, at));
                }

                offset = at + 1;
            }
        }

        public String get(String key) {
            return mMap.get(key);
        }

        public String get(String key, String defaultValue) {
            String result = mMap.get(key);
            return (result == null ? defaultValue : result);
        }

        public String toString() {
            return mMap.toString();
        }
    }

    private static final class NugenUEventThread extends Thread {
        /** Many to many mapping of string match to observer.
         *  Multimap would be better, but not available in android, so use
         *  an ArrayList where even elements are the String match and odd
         *  elements the corresponding UEventObserver observer */
        private final ArrayList<Object> mKeysAndObservers = new ArrayList<Object>();

        private final ArrayList<CustUEventObserver> mTempObserversToSignal =
                new ArrayList<CustUEventObserver>();

        public NugenUEventThread() {
            super("UEventObserver");
        }

        @Override
        public void run() {
            nativeSetup();

            while (true) {
                String message = nativeWaitForNextEvent();
                if (message != null) {
                	Debug.printI(TAG, "[NugenUEventThread] message");
                    sendEvent(message);
                }
            }
        }

        private void sendEvent(String message) {
        	Debug.printI(TAG, "[sendEvent] enter");
            synchronized (mKeysAndObservers) {
                final int N = mKeysAndObservers.size();
                Debug.printI(TAG, "[sendEvent] N = " + N);
                for (int i = 0; i < N; i += 2) {
                    final String key = (String)mKeysAndObservers.get(i);
                    if (message.contains(key)) {
                        final CustUEventObserver observer =
                                (CustUEventObserver)mKeysAndObservers.get(i + 1);
                        mTempObserversToSignal.add(observer);
                    }
                }
            }

            Debug.printI(TAG, "[sendEvent] mTempObserversToSignal isempty = " + mTempObserversToSignal.isEmpty());
            if (!mTempObserversToSignal.isEmpty()) {
                final UEvent event = new UEvent(message);
                final int N = mTempObserversToSignal.size();
                for (int i = 0; i < N; i++) {
                    final CustUEventObserver observer = mTempObserversToSignal.get(i);
                    observer.onUEvent(event);
                }
                mTempObserversToSignal.clear();
            }
        }

        public void addObserver(String match, CustUEventObserver observer) {
        	Debug.printI(TAG, "[addObserver] enter");
            synchronized (mKeysAndObservers) {
                mKeysAndObservers.add(match);
                mKeysAndObservers.add(observer);
            }
        }

        /** Removes every key/value pair where value=observer from mObservers */
        public void removeObserver(CustUEventObserver observer) {
            synchronized (mKeysAndObservers) {
                for (int i = 0; i < mKeysAndObservers.size(); ) {
                    if (mKeysAndObservers.get(i + 1) == observer) {
                        mKeysAndObservers.remove(i + 1);
                    } else {
                        i += 2;
                    }
                }
            }
        }
    }
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */

