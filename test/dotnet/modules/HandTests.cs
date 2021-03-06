﻿// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

using DotnetRunner;
using DotnetRunner.Data;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace DotnetModulesTests
{
    public class HandTests
    {
        public static IEnumerable<object[]> TestParameterSet { get; } = new[]
        {
            new ModuleTestParameters("./DiffSharpModule.dll", 1e-10 )
        }.Select(m => new object[] { m }).ToArray();

        // helper methods
        void CheckSimpleObjectiveCalculation(string moduleName, double tolerance, int times)
        {
            using (var moduleLoader = new ModuleLoader(moduleName))
            {
                var module = moduleLoader.GetHandTest();
                Assert.NotNull(module);
                var comparer = new TolerantDoubleComparer(tolerance);

                // Read instance
                var input = DataLoader.ReadHandInstance("handtest.txt", false);

                module.Prepare(input);
                module.CalculateObjective(times);

                var output = module.Output();

                var correctObjective = new[] { 1.65193147941611551e-01, -1.74542769272742593e-01, 1.54751161622253441e-01, -1.25651749731793605e-01, -4.25102935355075040e-02, -1.30665781132340175e-01 };
                Assert.Equal(correctObjective, output.Objective, comparer);
            }
        }

        void CheckSimpleJacobianCalculation(string moduleName, double tolerance, int times)
        {
            using (var moduleLoader = new ModuleLoader(moduleName))
            {
                var module = moduleLoader.GetHandTest();
                Assert.NotNull(module);
                var comparer = new TolerantDoubleComparer(tolerance);

                // Read instance
                var input = DataLoader.ReadHandInstance("handtest.txt", false);

                module.Prepare(input);
                module.CalculateJacobian(times);

                var output = module.Output();

                var correctJacobian = new[]
                {
                    new[] { -1.95512959341282745e-02, 1.18421319736437044e-02, -1.08140043450366714e-02, -1.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -2.21537299179855318e-02, 2.98023294354714807e-02, -8.89718384720838799e-03, -2.24887887916698764e-03 },
                    new[] { -6.81027576369377941e-03, 1.74108797580776152e-03, -6.54731655106268462e-02, 0.00000000000000000e+00, -1.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 1.42669175415592216e-02, 2.58519935565392558e-02, 2.30412622537484605e-03, 3.09895230315512067e-03 },
                    new[] { -4.09359630708754085e-02, 2.57836165362687400e-02, 2.95529253068245066e-03, 0.00000000000000000e+00, 0.00000000000000000e+00, -1.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -3.12350905279757118e-02, -9.40429974684049881e-03, -1.84753145105870202e-02, -3.09211880497457705e-04 },
                    new[] { -7.10032003143440671e-02, 5.14892578985870278e-02, -6.33408854067304267e-02, -1.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -3.45326893768281365e-02, -1.95437595587887036e-02, -1.29102628630682510e-02, 1.99738182488361877e-03, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00 },
                    new[] { -2.81346929141111299e-02, -4.50591977492771356e-02, -8.98189200086859235e-02, 0.00000000000000000e+00, -1.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 2.42024596962444292e-02, -1.84392814127634017e-02, 9.76606245804552756e-03, -1.42570914120403679e-03, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00 },
                    new[] { -2.54484136783155838e-02, 6.23981738234076627e-02, 2.07440902671436785e-02, 0.00000000000000000e+00, 0.00000000000000000e+00, -1.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -3.10675692993888850e-03, 6.52740542794425036e-03, 8.49553578389919579e-03, -4.35490319929073976e-04, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00 }
                };

                Assert.Equal(correctJacobian.Length, output.Jacobian.Length);
                for (int i = 0; i < correctJacobian.Length; ++i)
                    Assert.Equal(correctJacobian[i], output.Jacobian[i], comparer);
            }
        }
        void CheckComplicatedObjectiveCalculation(string moduleName, double tolerance, int times)
        {
            using (var moduleLoader = new ModuleLoader(moduleName))
            {
                var module = moduleLoader.GetHandTest();
                Assert.NotNull(module);
                var comparer = new TolerantDoubleComparer(tolerance);

                // Read instance
                var input = DataLoader.ReadHandInstance("hand_complicatedtest.txt", true);

                module.Prepare(input);
                module.CalculateObjective(times);

                var output = module.Output();

                var correctObjective = new[] { 1.56187661696463698e-01, -1.49300526003322220e-01, 1.72238089826454832e-01, -9.88770451849596554e-02, -1.61238035462101248e-02, -1.97586768465579654e-01 };
                Assert.Equal(correctObjective, output.Objective, comparer);
            }
        }

        void CheckComplicatedJacobianCalculation(string moduleName, double tolerance, int times)
        {
            using (var moduleLoader = new ModuleLoader(moduleName))
            {
                var module = moduleLoader.GetHandTest();
                Assert.NotNull(module);
                var comparer = new TolerantDoubleComparer(tolerance);

                // Read instance
                var input = DataLoader.ReadHandInstance("hand_complicatedtest.txt", true);

                module.Prepare(input);
                module.CalculateJacobian(times);

                var output = module.Output();

                var correctJacobian = new[]
                {
                    new[] { 3.19994705650605837e-03, 5.94121036324768426e-03, -3.16780414139280486e-02, 2.51273846313546725e-02, -3.43751692490045363e-02, -1.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -2.67300459304900973e-03, -2.05375964421033422e-04, -0.00000000000000000e+00, -0.00000000000000000e+00 },
                    new[] { -5.83664316352128232e-03, -2.25087272196717869e-02, -1.17603133455238852e-02, -6.73431005210687033e-03, -7.93824304538508912e-02, 0.00000000000000000e+00, -1.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 2.51903206295353913e-03, 6.35746365607068199e-03, 0.00000000000000000e+00, 0.00000000000000000e+00 },
                    new[] { 5.26539698350125818e-03, -7.98030902470658887e-04, -4.00357012848860105e-02, 4.48594779455911905e-02, 1.08995867664168408e-02, 0.00000000000000000e+00, 0.00000000000000000e+00, -1.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -1.65741767881614870e-03, 2.11520284492772506e-03, -0.00000000000000000e+00, -0.00000000000000000e+00 },
                    new[] { 6.09513107611836524e-03, 4.72340259356063275e-03, -2.21424420388885296e-02, 3.97751453573731983e-02, -8.70476400071599210e-02, -1.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, -3.33849263402929107e-03, 1.28209220660817119e-03, -0.00000000000000000e+00, -0.00000000000000000e+00, 1.37205123955743335e-03, -5.45177232375653083e-03, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00 },
                    new[] { -1.30273083127474543e-02, 5.73648537699011918e-03, -9.03798668988376082e-03, -1.01785098875940283e-02, -4.37081649532052238e-02, 0.00000000000000000e+00, -1.00000000000000000e+00, 0.00000000000000000e+00, 9.53458231126915585e-04, 4.01972269593312698e-03, 0.00000000000000000e+00, 0.00000000000000000e+00, -9.33621518070085026e-04, -1.38222424349224984e-03, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00, 0.00000000000000000e+00 },
                    new[] { -1.97054905573765815e-02, 8.52371287096032049e-03, 1.63019777955282150e-03, 7.14646422504415374e-02, 2.94403102374449152e-02, 0.00000000000000000e+00, 0.00000000000000000e+00, -1.00000000000000000e+00, 3.24947244952083786e-03, 8.49942589390221431e-04, -0.00000000000000000e+00, -0.00000000000000000e+00, -5.87537490240527642e-03, 2.95494237613404438e-03, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00, -0.00000000000000000e+00 }
                };

                Assert.Equal(correctJacobian.Length, output.Jacobian.Length);
                for (int i = 0; i < correctJacobian.Length; ++i)
                    Assert.Equal(correctJacobian[i], output.Jacobian[i], comparer);
            }
        }

        [Theory]
        [MemberData(nameof(TestParameterSet))]
        public void Load(ModuleTestParameters testParameters)
        {
            using (var moduleLoader = new ModuleLoader(testParameters.ModuleName))
            {
                var test = moduleLoader.GetGMMTest();
                Assert.NotNull(test);
            }
        }

        [Theory]
        [MemberData(nameof(TestParameterSet))]
        public void ObjectiveSimpleCalculationCorrectness(ModuleTestParameters testParameters)
        {
            CheckSimpleObjectiveCalculation(testParameters.ModuleName, testParameters.Tolerance, times: 1);
        }

        [Theory]
        [MemberData(nameof(TestParameterSet))]
        public void ObjectiveSimpleMultipleTimesCalculationCorrectness(ModuleTestParameters testParameters)
        {
            CheckSimpleObjectiveCalculation(testParameters.ModuleName, testParameters.Tolerance, times: 3);
        }

        [Theory]
        [MemberData(nameof(TestParameterSet))]
        public void JacobianSimpleCalculationCorrectness(ModuleTestParameters testParameters)
        {
            CheckSimpleJacobianCalculation(testParameters.ModuleName, testParameters.Tolerance, times: 1);
        }

        [Theory]
        [MemberData(nameof(TestParameterSet))]
        public void JacobianSimpleMultipleTimesCalculationCorrectness(ModuleTestParameters testParameters)
        {
            CheckSimpleJacobianCalculation(testParameters.ModuleName, testParameters.Tolerance, times: 3);
        }

        [Theory]
        [MemberData(nameof(TestParameterSet))]
        public void ObjectiveComplicatedCalculationCorrectness(ModuleTestParameters testParameters)
        {
            CheckComplicatedObjectiveCalculation(testParameters.ModuleName, testParameters.Tolerance, times: 1);
        }

        [Theory]
        [MemberData(nameof(TestParameterSet))]
        public void ObjectiveComplicatedMultipleTimesCalculationCorrectness(ModuleTestParameters testParameters)
        {
            CheckComplicatedObjectiveCalculation(testParameters.ModuleName, testParameters.Tolerance, times: 3);
        }

        [Theory]
        [MemberData(nameof(TestParameterSet))]
        public void JacobianComplicatedCalculationCorrectness(ModuleTestParameters testParameters)
        {
            CheckComplicatedJacobianCalculation(testParameters.ModuleName, testParameters.Tolerance, times: 1);
        }

        [Theory]
        [MemberData(nameof(TestParameterSet))]
        public void JacobianComplicatedMultipleTimesCalculationCorrectness(ModuleTestParameters testParameters)
        {
            CheckComplicatedJacobianCalculation(testParameters.ModuleName, testParameters.Tolerance, times: 3);
        }

        [Theory]
        [MemberData(nameof(TestParameterSet))]
        public void ObjectiveSimpleRunsMultipleTimes(ModuleTestParameters testParameters)
        {
            using (var moduleLoader = new ModuleLoader(testParameters.ModuleName))
            {
                var module = moduleLoader.GetHandTest();
                Assert.NotNull(module);

                // Read instance
                var input = DataLoader.ReadHandInstance("handtest.txt", false);

                module.Prepare(input);

                Assert.True(Utils.CanObjectiveRunMultipleTimes(module.CalculateObjective));
            }
        }

        [Theory]
        [MemberData(nameof(TestParameterSet))]
        public void JacobianSimpleRunsMultipleTimes(ModuleTestParameters testParameters)
        {
            using (var moduleLoader = new ModuleLoader(testParameters.ModuleName))
            {
                var module = moduleLoader.GetHandTest();
                Assert.NotNull(module);

                // Read instance
                var input = DataLoader.ReadHandInstance("handtest.txt", false);

                module.Prepare(input);

                Assert.True(Utils.CanObjectiveRunMultipleTimes(module.CalculateJacobian));
            }
        }

        [Theory]
        [MemberData(nameof(TestParameterSet))]
        public void ObjectiveComplicatedRunsMultipleTimes(ModuleTestParameters testParameters)
        {
            using (var moduleLoader = new ModuleLoader(testParameters.ModuleName))
            {
                var module = moduleLoader.GetHandTest();
                Assert.NotNull(module);

                // Read instance
                var input = DataLoader.ReadHandInstance("hand_complicatedtest.txt", true);

                module.Prepare(input);

                Assert.True(Utils.CanObjectiveRunMultipleTimes(module.CalculateObjective));
            }
        }

        [Theory]
        [MemberData(nameof(TestParameterSet))]
        public void JacobianComplicatedRunsMultipleTimes(ModuleTestParameters testParameters)
        {
            using (var moduleLoader = new ModuleLoader(testParameters.ModuleName))
            {
                var module = moduleLoader.GetHandTest();
                Assert.NotNull(module);

                // Read instance
                var input = DataLoader.ReadHandInstance("hand_complicatedtest.txt", true);

                module.Prepare(input);

                Assert.True(Utils.CanObjectiveRunMultipleTimes(module.CalculateJacobian));
            }
        }
    }
}
