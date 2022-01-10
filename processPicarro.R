# IsoBank Trial 

# Read in libraries
	library('ncdf4')

# Read in CONCH field data
	ncfile<-'~/Desktop/Desktop_Stuff/ICECHIP/HIDS2406_Water-Vapor-Isotopes_20210601.nc'
	nc<-nc_open(ncfile)
	time<-ncvar_get(nc,'time')
	dD<-ncvar_get(nc,'dD')
	d18O<-ncvar_get(nc,'d18O')
	vmr<-ncvar_get(nc,'vmr')
	nc_close(nc)
	
# Make hourly averages
	hours<-seq(min(time)*24,max(time)*24)
	dDh<-hours*NA
	d18Oh<-hours*NA
	vmrh<-hours*NA
	for (i in 1:length(hours)){
		id<-which(abs(hours[i]-time*24)<.5)
		if( length(id)>=30){
			dDh[i]<-sum(dD[id]*vmr[id], na.rm=TRUE)/sum(vmr[id], na.rm=TRUE)
			d18Oh[i]<-sum(d18O[id]*vmr[id], na.rm=TRUE)/sum(vmr[id], na.rm=TRUE)
			vmrh[i]<-mean(vmr[id], na.rm=TRUE)
		}
	}
	timeh<-hours/24

# Create a Lab name, lab_sample_id, user_sample_id, sample_measurement_id
	date<-as.Date(timeh, origin=as.Date("2021-01-01")) # form YYYY-MM-DD
	hour<-(timeh-floor(timeh))*24
	lab_sample_id<-vector(length=length(date))
	collection_date<-vector(length=length(date))
	for (i in 1:length(date)){
			tmp<-unlist(strsplit(toString(date[i]),'-'))
			day<-paste(tmp[1],tmp[2],tmp[3], sep='')
			if (hour[i]<10){
				time<-paste('0',toString(round(hour[i],0)),'0000Z', sep='')
			} else {
				time<-paste(toString(round(hour[i],0)),'0000Z', sep='')
			}
			lab_sample_id[i]<-paste('HIDS2406',day,time, sep='_')
			collection_date[i]<-paste(day,time,sep='_')
	}
	
	lab<-rep("NCAR WVISO", length(date))
	lab_sample_id<-lab_sample_id
	user_sample_id<-lab_sample_id
	sample_measurement_id<-lab_sample_id
	collected_sample_size_parameter<-rep('mole fraction of water vapor in air (ppmv)', length(date))
	collected_sample_size_measurement<-round(vmrh,0)
	#collected_sample_size_unit<-rep('ppmv', length(date))
	analysis_normalization<-rep('3 point', length(date))
	analysis_type<-rep('compound specific', length(date))
	other_instrumentation<-rep('Picarro L2130-i', length(date))
	instrumentation<-rep('CRDS', length(date))
	analytical_matrix<-rep('water', length(date))
	material_type<-rep('inorganic-organic composite : whole air', length(date))
	preparation_step<-rep('none', length(date))
	collection_date<-collection_date
	collection_source<-rep('field', length(date))
	experimental_manipulation<-rep('none',length(date))
	investigator_name<-rep('Adriana Bailey', length(date))
	investigator_email<-rep('abailey@ucar.edu', length(date))
	investigator_orcid<-rep('0000-0002-2614-1560', length(date))
	collection_decimal_latitude<-rep(40+ 26/60 + 46.5/60/60, length(date))
	geodetic_datum<-rep('unknown', length(date))
	collection_locality<-rep('CSU-CHILL National Weather Radar Facility', length(date))
	max_distance_above_surface_meters<-rep(2, length(date))
	max_elevation_meters<-rep(1432, length(date))
	min_distance_above_surface_meters<-rep(2, length(date))
	min_elevation_meters<-rep(1432, length(date))
	collection_decimal_longitude<-rep(104.63708, length(date))
	water_source<-rep('vapor', length(date))
	water_phase<-rep('gas', length(date))
	d2H<-round(dDh,1)
	d2H_measurement_scale<-rep('VSMOW', length(date))
	d2H_measurement_unit<-rep('per mille', length(date))
	d18O<-round(d18Oh,1)
	d18O_measurement_scale<-rep('VSMOW', length(date))
	d18O_measurement_unit<-rep('per mille', length(date))

	data<-as.data.frame(cbind(lab,lab_sample_id, user_sample_id,sample_measurement_id, collected_sample_size_parameter,collected_sample_size_measurement, analysis_normalization, analysis_type, other_instrumentation,instrumentation, analytical_matrix, material_type,preparation_step,collection_date,collection_source, experimental_manipulation,investigator_name,investigator_email,investigator_orcid , collection_decimal_latitude,geodetic_datum ,collection_locality, max_distance_above_surface_meters,max_elevation_meters,  min_distance_above_surface_meters, min_elevation_meters,collection_decimal_longitude,water_source,  water_phase, d2H,d2H_measurement_scale, d2H_measurement_unit,  d18O, d18O_measurement_scale, d18O_measurement_unit))
	
 	ok<-which(is.na(dDh)==FALSE)	
 	data<-data[ok,]
		
# Output to CSV
	write.csv(data,'~/Desktop/CONCH_hourly_for_IsoBank.csv', row.names=FALSE)