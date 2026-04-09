package com.alphacaddie;

import com.alphacaddie.config.DataGolfProperties;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;

@SpringBootApplication
@EnableConfigurationProperties(DataGolfProperties.class)
public class AlphaCaddieApplication {

    public static void main(String[] args) {
        SpringApplication.run(AlphaCaddieApplication.class, args);
    }
}
