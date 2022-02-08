/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: IntentResult
 * Brief: Encapsulates the result of a barcode scan invoked through IntentIntegrator
 *
 * Create Date: 07/08/2015
 * $Revision: 24626 $
 * $Author: AdamChen $
 * $Id: IntentResult.java 24626 2015-11-23 10:15:10Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.globaltools;

/**
 * <p>
 * Encapsulates the result of a barcode scan invoked through
 * {@link IntentIntegrator}.
 * </p>
 * 
 * @author Sean Owen
 */
public final class IntentResult
{

    private final String contents;
    
    private final String formatName;
    
    private final byte[] rawBytes;
    
    private final Integer orientation;

    private final String errorCorrectionLevel;

    /**
     * 
     */
    IntentResult()
    {
        this(null, null, null, null, null);
    }

    /**
     * 
     * @param contents
     * @param formatName
     * @param rawBytes
     * @param orientation
     * @param errorCorrectionLevel
     */
    IntentResult(
            String contents, String formatName, byte[] rawBytes,
            Integer orientation, String errorCorrectionLevel)
    {
        this.contents = contents;
        this.formatName = formatName;
        this.rawBytes = rawBytes;
        this.orientation = orientation;
        this.errorCorrectionLevel = errorCorrectionLevel;
    }

    /**
     * @return raw content of barcode
     */
    public String getContents()
    {
        return contents;
    }

    /**
     * @return name of format, like "QR_CODE", "UPC_A". See
     *         {@code BarcodeFormat} for more format names.
     */
    public String getFormatName()
    {
        return formatName;
    }

    /**
     * @return raw bytes of the barcode content, if applicable, or null
     *         otherwise
     */
    public byte[] getRawBytes()
    {
        return rawBytes;
    }

    /**
     * @return rotation of the image, in degrees, which resulted in a successful
     *         scan. May be null.
     */
    public Integer getOrientation()
    {
        return orientation;
    }

    /**
     * @return name of the error correction level used in the barcode, if
     *         applicable
     */
    public String getErrorCorrectionLevel()
    {
        return errorCorrectionLevel;
    }

    /**
     * 
     */
    @Override
    public String toString()
    {
        StringBuilder dialogText = new StringBuilder(100);
        dialogText.append("Format: ").append(formatName).append('\n');
        dialogText.append("Contents: ").append(contents).append('\n');
        int rawBytesLength = 0;
        
        if(rawBytes != null)
        {
            rawBytesLength = rawBytes.length;
        }
        
        dialogText.append("Raw bytes: (").append(rawBytesLength)
                .append(" bytes)\n");
        dialogText.append("Orientation: ").append(orientation).append('\n');
        dialogText.append("EC level: ").append(errorCorrectionLevel)
                .append('\n');
        return dialogText.toString();
    }

}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
