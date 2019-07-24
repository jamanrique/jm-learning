cd("C://Users/jaman//Documents//GitHub//jm-learning//kaggle/crimes-in-boston")
using CSV, DataFrames
boston_cr = CSV.read("crime.csv",copycols=true)
crimes_master = CSV.read("offense_codes.csv",copycols=true)

# Limpiar el objeto crimes_master
unique!(crimes_master, :CODE)
sort!(crimes_master, :CODE)

# Unir boston_cr con crimes_master
boston_cr = join(boston_cr,crimes_master, on = :OFFENSE_CODE => :CODE, kind = :left)

# Creación de función summary, para conocer tipos de la variable
summary(df) = hcat(names(df),eltypes(df),vec(convert(Matrix,first(boston_cr,1))))
summary(boston_cr)
