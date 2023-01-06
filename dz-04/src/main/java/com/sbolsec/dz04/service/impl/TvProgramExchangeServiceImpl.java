package com.sbolsec.dz04.service.impl;

import com.sbolsec.dz04.service.TvProgramExchangeService;
import com.sbolsec.dz04.dto.Tv;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.net.URI;

@Service
public class TvProgramExchangeServiceImpl implements TvProgramExchangeService {

    private static final URI RTL_URI = URI.create("https://tvprofil.net/xmltv/data/rtl.hr/weekly_rtl.hr_tvprofil.net.xml");

    private final RestTemplate restTemplate;

    public TvProgramExchangeServiceImpl(RestTemplateBuilder builder) {
        this.restTemplate = builder.defaultHeader(HttpHeaders.ACCEPT, MediaType.APPLICATION_XML_VALUE).build();
    }

    @Override
    public Tv getTvProgrammes() {
        return restTemplate.getForObject(RTL_URI, Tv.class);
    }

}
