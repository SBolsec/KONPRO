package com.sbolsec.dz04;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;

@EnableAsync
@EnableScheduling
@SpringBootApplication
public class Dz04Application {

    public static void main(String[] args) {
        SpringApplication.run(Dz04Application.class, args);
    }

}
