package com.example.codegenerator.structures;

import java.util.Objects;

public abstract class GTKWidget {
    private String id; //indien in fxml expliciet met :id wordt gewerkt
    private Integer id_hash;  //interne hash value van widget
    private String name;

    private double layoutX;
    private double layoutY;

    public String gtkHsCode(){ return "" ; }

    public GTKWidget(){

    }

    public GTKWidget(String id, Integer id_hash, String name) {
        this.id = id;
        this.id_hash = id_hash;
        this.name = name;
    }

    public GTKWidget(String id, Integer id_hash, String name, double layoutX, double layoutY) {
        this.id = id;
        this.id_hash = id_hash;
        this.name = name;
        this.layoutX = layoutX;
        this.layoutY = layoutY;
    }

    public static String makeName(String id, Integer id_hash, String name){
        return Objects.requireNonNullElseGet(id, () -> name + "_" + id_hash);
    }

    public double getLayoutX() {
        return layoutX;
    }

    public void setLayoutX(double layoutX) {
        this.layoutX = layoutX;
    }

    public double getLayoutY() {
        return layoutY;
    }

    public void setLayoutY(double layoutY) {
        this.layoutY = layoutY;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public Integer getId_hash() {
        return id_hash;
    }

    public void setId_hash(Integer id_hash) {
        this.id_hash = id_hash;
    }
}
