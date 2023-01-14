package com.sbolsec.lab4.dto;

import jakarta.xml.bind.annotation.*;
import lombok.Data;

@Data
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "channel")
public class Channel {

    @XmlAttribute(name = "id")
    private String id;

    @XmlElement(name = "display-name")
    private String displayName;

    @XmlElement(name = "url")
    private String url;

    @XmlElement(name = "icon")
    private Icon icon;

}
