package com.sbolsec.dz04.adapter;

import jakarta.xml.bind.annotation.adapters.XmlAdapter;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class LocalDateTimeAdapter extends XmlAdapter<String, LocalDateTime> {

    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyyMMddHHmmss Z");

    @Override
    public LocalDateTime unmarshal(String value) throws Exception {
        return LocalDateTime.parse(value, FORMATTER);
    }

    @Override
    public String marshal(LocalDateTime value) throws Exception {
        return value.toString();
    }

}
