package com.sbolsec.dz04.dto;

import jakarta.xml.bind.annotation.*;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
@XmlRootElement(name = "tv")
@XmlAccessorType(XmlAccessType.FIELD)
public class Tv {

    @XmlElement
    private Channel channel;

    @XmlElement(name = "programme")
    private List<Programme> programmeList = new ArrayList<>();

}
