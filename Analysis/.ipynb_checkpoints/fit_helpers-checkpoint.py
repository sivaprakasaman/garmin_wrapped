#Fit file helper functions to parse data
#Written by: Andrew Sivaprakasam
#Last Updated: Jan 2023

## TODO:
# - Return a list of non-empty activities in a fit file
# - Return a list with a metric (e.g. heart rate, lat, lon, with time in one column)
# - Various Visualizations? Shaded plots, scatter plots, nicely formatted, idk...

import numpy as np
import pandas as pd
import fitparse as fp

# Return a list of non-empty activities

def fit_to_df(act_fit, remove_unknowns=1):
    """
    act_fit -> String with fit file name. Assumes the fit file is in the current path
    
    Returns an ndarray with all unique message IDs
    Returns a dataframe with all non-empty messages (and values)
    """
    # Parse the fit file
    fitfile = fp.FitFile(act_fit)

    # Extract the data from the fit file
    data = []
    tstamp = None;
    for record in fitfile.get_messages():
        for record_data in record:
            if record_data.value is not None:
                if record_data.name == 'timestamp':
                    tstamp = record_data.value;
                    
                if remove_unknowns: 
                    if 'unknown' not in record_data.name:
                        data.append([tstamp, record_data.name, record_data.value]);
                else:
                    data.append([tstamp, record_data.name, record_data.value]);

    # Create a Pandas data frame from the data
    df = pd.DataFrame(data, columns = ['Time','Field','Value']);
    uniques = df['Field'].unique();
    
    return uniques, df;

def get_record_by_field(df, field = 'heart_rate', units = 's'):
    """
    df -> dataFrame thats in the format returned by fit_to_df
    field -> String with the name of the field you're trying to get
    units -> preferred time unit 's' or 'm'. Defaults to seconds, but you can put in a dummy string if you want the raw timestamp
    
    returns
    dataframe with first column time in format specified by units, second column is data
    
    NOTE: May not give right activity time if rtype is not logged at the beginning! Maybe should fix this!
    """
        
    df_out = df;
    df_out = df_out[df['Field'] == field];
    
    df_out.drop(columns=["Field"], inplace=True);
    
    #calculate time difference
    times = df_out['Time']-df_out['Time'].iloc[0];
    times = times.astype('timedelta64[s]');
    
    if units == 's':
        df_out['Time'] = times;
    elif units == 'm':
        df_out['Time'] = times/60;

    return df_out;
