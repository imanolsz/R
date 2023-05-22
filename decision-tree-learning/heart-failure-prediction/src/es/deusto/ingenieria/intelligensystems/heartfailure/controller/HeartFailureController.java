package es.deusto.ingenieria.intelligensystems.heartfailure.controller;

import com.oracle.truffle.api.TruffleLanguage;

import es.deusto.ingenieria.intelligensystems.heartfailure.domain.Patient.ChestPainType;
import es.deusto.ingenieria.intelligensystems.heartfailure.domain.Patient.RestingElectrocardiogram;
import es.deusto.ingenieria.intelligensystems.heartfailure.domain.Patient.STSlope;
import es.deusto.ingenieria.intelligensystems.heartfailure.domain.Patient.Sex;

public class HeartFailureController {
	
	public boolean predict(short age, 
						   Sex sex,
						   ChestPainType chestPainType,
						   int restingBloodPressuere,
						   int cholesterol,
						   boolean hasFastingBloodSugar,
						   RestingElectrocardiogram restingECG,
						   int maximumHeartRate,
						   boolean hasExerciseAngina,
						   short oldpeak,
						   STSlope stSlope) {
							
		if (stSlope == STSlope.UP &&
				(chestPainType == ChestPainType.ATA || chestPainType == ChestPainType.NAP || chestPainType == ChestPainType.TA)) {
			return false;
		}
		
		if ((stSlope == STSlope.DOWN || stSlope == STSlope.FLAT) &&
				(chestPainType == ChestPainType.ATA || chestPainType == ChestPainType.NAP || chestPainType == ChestPainType.TA) &&
				sex == Sex.M &&
				restingECG == RestingElectrocardiogram.LVH &&
				age < 55) {
			return false;
		}
		
		if (stSlope == STSlope.UP &&
				chestPainType == ChestPainType.ASY &&
				!hasExerciseAngina &&
				!hasFastingBloodSugar) {
			return false;
		}
		
		if ((stSlope == STSlope.DOWN || stSlope == STSlope.FLAT) &&
				(chestPainType == ChestPainType.ATA || chestPainType == ChestPainType.NAP || chestPainType == ChestPainType.TA) &&
				sex == Sex.F) {
			return true;
		}
		
		if ((stSlope == STSlope.DOWN || stSlope == STSlope.FLAT) &&
				(chestPainType == ChestPainType.ATA || chestPainType == ChestPainType.NAP || chestPainType == ChestPainType.TA) &&
				sex == Sex.M &&
				restingECG == RestingElectrocardiogram.LVH &&
				age >= 55) {
			return true;
		}
		
		if (stSlope == STSlope.UP &&
				chestPainType == ChestPainType.ASY &&
				!hasExerciseAngina &&
				hasFastingBloodSugar) {
			return true;
		}
		
		if (stSlope == STSlope.UP &&
				chestPainType == ChestPainType.ASY &&
				hasExerciseAngina) {
			return true;
		}
		
		if ((stSlope == STSlope.DOWN || stSlope == STSlope.FLAT) &&
				(chestPainType == ChestPainType.ATA || chestPainType == ChestPainType.NAP || chestPainType == ChestPainType.TA) &&
				sex == Sex.M &&
				(restingECG == RestingElectrocardiogram.NORMAL || restingECG == RestingElectrocardiogram.ST)) {
			return true;
		}
		
		if ((stSlope == STSlope.DOWN || stSlope == STSlope.FLAT) &&
				chestPainType == ChestPainType.ASY) {
			return true;
		}
		
		return true; // Default prediction if no rule matches		
	}
}